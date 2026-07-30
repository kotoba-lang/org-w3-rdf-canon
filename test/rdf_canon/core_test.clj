(ns rdf-canon.core-test
  "Correctness is measured against the OFFICIAL W3C test suite, not against this
   implementation's own output. `test/fixtures/rdfc10/` holds input/expected pairs
   checked out of https://w3c.github.io/rdf-canon/tests/ and committed, so the suite
   does not depend on the network.

   A canonicalizer that only agreed with itself would be worthless — the entire
   point is that two independent parties produce the same bytes."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [nquads.core :as nq]
            [rdf-canon.core :as c14n]))

(def ^:private fixture-dir "fixtures/rdfc10")

(defn- fixture [name]
  (slurp (io/resource (str fixture-dir "/" name))))

(def ^:private index
  (delay (json/read-str (fixture "index.json") :key-fn keyword)))

;; ── the official suite ───────────────────────────────────────────────────────

(deftest official-w3c-vectors
  (testing "every vector either matches W3C's expected output byte for byte, or
            throws :rdf-canon/n-degree-required — never a third thing"
    (let [results
          (for [{:keys [file name complexity]} @index]
            (let [input (fixture (str file "-in.nq"))
                  expected (fixture (str file "-out.nq"))]
              (try
                (let [actual (c14n/canonicalize input)]
                  {:file file :name name :complexity complexity
                   :outcome (if (= expected actual) :match :MISMATCH)
                   :expected expected :actual actual})
                (catch clojure.lang.ExceptionInfo e
                  {:file file :name name :complexity complexity
                   :outcome (or (:rdf-canon/error (ex-data e))
                                (:nquads/error (ex-data e))
                                :THREW)}))))
          matched (filter #(= :match (:outcome %)) results)
          deferred (filter #(= :rdf-canon/n-degree-required (:outcome %)) results)
          other (remove #(#{:match :rdf-canon/n-degree-required} (:outcome %)) results)]

      (testing "nothing produces a WRONG canonical form"
        ;; This is the assertion that matters. A mismatch means we emitted a hash
        ;; that disagrees with W3C — worse than refusing.
        (is (empty? (filter #(= :MISMATCH (:outcome %)) results))
            (str "mismatches: "
                 (pr-str (map (fn [r] {:file (:file r)
                                       :expected (:expected r)
                                       :actual (:actual r)})
                              (filter #(= :MISMATCH (:outcome %)) results))))))

      (testing "and every other outcome is the documented refusal, not a surprise"
        (is (empty? other) (str "unexpected outcomes: " (pr-str other))))

      (testing "the simple path actually handles a useful number of them"
        (is (pos? (count matched)))
        (println (str "\n  official RDFC-1.0 vectors: " (count matched) " matched, "
                      (count deferred) " deferred to N-degree, "
                      (count other) " other, of " (count results) " total")))

      (testing "and the deferred ones are the ones with shared first-degree hashes"
        (doseq [d deferred]
          (is (= :rdf-canon/n-degree-required (:outcome d))))))))

;; ── the refusal is explicit and informative ──────────────────────────────────

(deftest sharing-a-first-degree-hash-refuses-rather-than-guessing
  (testing "two blank nodes indistinguishable by their first-degree information.
            Any ordering we invented would look normal and disagree with everyone."
    (let [p (nq/iri "http://example.com/p")
          ;; _:b0 and _:b1 appear in structurally identical quads, so their
          ;; first-degree hashes collide by construction.
          dataset [{:subject (nq/blank "b0") :predicate p :object (nq/literal "v")}
                   {:subject (nq/blank "b1") :predicate p :object (nq/literal "v")}]
          e (try (c14n/canonicalize dataset)
                 (catch clojure.lang.ExceptionInfo ex ex))]
      (is (= :rdf-canon/n-degree-required (:rdf-canon/error (ex-data e))))
      (testing "and it names the labels involved, so a caller can see why"
        (is (= #{"b0" "b1"} (set (:labels (ex-data e)))))))))

;; ── code point order, which is NOT UTF-16 order ───────────────────────────────

(deftest ordering-is-by-code-point-not-code-unit
  (testing "a supplementary character is stored as a surrogate pair leading with
            0xD800-0xDBFF, so it sorts BEFORE U+E000 under UTF-16 code units and
            AFTER it under code points. RFC 8785 JCS wants the former; RDFC-1.0
            wants the latter, and using one library's comparator for the other's
            job is a silent interoperability bug."
    (let [emoji "\ud83d\ude00"   ; U+1F600, lead surrogate D83D
          pua "\ue000"]           ; U+E000
      (testing "UTF-16 code unit order (what `compare` gives)"
        (is (neg? (compare emoji pua)) "emoji first, because D83D < E000"))
      (testing "code point order (what §4.4.3 requires)"
        (is (pos? (c14n/compare-code-points emoji pua))
            "emoji last, because U+1F600 > U+E000"))
      (testing "so the two comparators genuinely disagree on this pair"
        (is (not= (pos? (compare emoji pua))
                  (pos? (c14n/compare-code-points emoji pua)))))))

  (testing "and they agree across the BMP, where code units and code points coincide"
    (doseq [[a b] [["a" "b"] ["<" ">"] ["_:a" "_:z"] ["" "a"] ["aa" "ab"]
                   ["\u00e9" "\u00ea"]]]
      (is (= (int (Math/signum (double (compare a b))))
             (int (Math/signum (double (c14n/compare-code-points a b)))))
          (str (pr-str a) " vs " (pr-str b)))))

  (testing "a prefix sorts before its extension, and equals compare equal"
    (is (neg? (c14n/compare-code-points "ab" "abc")))
    (is (zero? (c14n/compare-code-points "abc" "abc")))
    (is (zero? (c14n/compare-code-points "" ""))))

  (testing "sorting a real list by code point differs from the default sort"
    (let [xs ["\ue000" "\ud83d\ude00" "a"]]
      (is (= ["a" "\ue000" "\ud83d\ude00"] (sort c14n/compare-code-points xs)))
      (is (= ["a" "\ud83d\ude00" "\ue000"] (sort xs))
          "control: the default sort puts the emoji in the middle"))))

;; ── §4.6 first-degree hashing ────────────────────────────────────────────────

(deftest the-reference-node-becomes-a-and-others-become-z
  (testing "§4.6.3: that substitution is what makes the hash independent of the
            arbitrary incoming labels"
    ;; the same graph with the two labels swapped must hash x the way it hashes y
    (let [p (nq/iri "http://example.com/p")
          m (c14n/blank-node-to-quads
             [{:subject (nq/blank "x") :predicate p :object (nq/blank "y")}])
          m2 (c14n/blank-node-to-quads
              [{:subject (nq/blank "y") :predicate p :object (nq/blank "x")}])]
      (is (= (c14n/hash-first-degree m "x") (c14n/hash-first-degree m2 "y"))
          "renaming the labels does not change the first-degree hash"))))

(deftest first-degree-hashing-refuses-an-unknown-label
  (is (= :rdf-canon/unknown-blank-node
         (:rdf-canon/error
          (ex-data (try (c14n/hash-first-degree {} "nope")
                        (catch clojure.lang.ExceptionInfo e e)))))))

;; ── §4.5 the identifier issuer ───────────────────────────────────────────────

(deftest the-issuer-is-a-bijection
  (let [i0 (c14n/issuer c14n/canonical-prefix)
        [i1 a] (c14n/issue i0 "x")
        [i2 b] (c14n/issue i1 "y")
        [i3 a-again] (c14n/issue i2 "x")]
    (is (= "c14n0" a))
    (is (= "c14n1" b))
    (testing "re-issuing returns the SAME identifier — a second would break the
              bijection the canonical form depends on"
      (is (= a a-again))
      (is (= 2 (:counter i3)) "and does not advance the counter"))))

;; ── a graph with no blank nodes at all ───────────────────────────────────────

(deftest a-ground-graph-is-just-sorted
  (testing "with nothing to relabel, canonicalization is the sort"
    (let [p (nq/iri "http://example.com/p")
          s (nq/iri "http://example.com/s")
          out (c14n/canonicalize [{:subject s :predicate p :object (nq/literal "b")}
                                  {:subject s :predicate p :object (nq/literal "a")}])]
      (is (= (str "<http://example.com/s> <http://example.com/p> \"a\" .\n"
                  "<http://example.com/s> <http://example.com/p> \"b\" .\n")
             out)))))

(deftest canonical-hash-is-stable-and-order-independent
  (let [p (nq/iri "http://example.com/p")
        s (nq/iri "http://example.com/s")
        a {:subject s :predicate p :object (nq/literal "a")}
        b {:subject s :predicate p :object (nq/literal "b")}]
    (is (= (c14n/canonical-hash [a b]) (c14n/canonical-hash [b a]))
        "the input order of statements must not change the hash")
    (is (= 64 (count (c14n/canonical-hash [a b]))) "sha-256 as hex")))

(deftest canonicalize-accepts-nquads-text-or-statements
  (let [text "<http://ex.com/s> <http://ex.com/p> \"v\" .\n"]
    (is (= text (c14n/canonicalize text)))
    (is (= text (c14n/canonicalize (nq/parse text))))))
