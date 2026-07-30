(ns rdf-canon.core
  "RDF Dataset Canonicalization (RDFC-1.0) — [W3C REC](https://www.w3.org/TR/rdf-canon/).

   Second layer of the `-rdfc-` cryptosuite stack. `nquads.core` fixes the bytes for
   a term; this fixes which *blank node labels* those bytes carry, so that two
   parties holding the same graph produce the same hash even though blank node
   labels are arbitrary.

   ## What is implemented, and what refuses rather than guesses

   The simple path is complete: state construction, first-degree hashing, and the
   issuing of canonical identifiers to blank nodes whose first-degree hash is
   unique (§4.4.3 steps 1-4, 6-7).

   **Step 5 — the N-degree hashing that resolves blank nodes SHARING a first-degree
   hash — is not implemented, and `canonicalize` throws
   `:rdf-canon/n-degree-required` when it is reached.**

   That refusal is the whole design decision here. A canonicalizer that fell back
   to some other ordering for those graphs would return a hash that looks perfectly
   normal and disagrees with every other implementation — silently, and only for
   graphs with interconnected blank nodes, which is precisely the non-trivial case.
   Throwing means a caller learns the limit at the moment it matters instead of
   discovering it as an unverifiable signature later.

   The reason it is not implemented is also worth stating: the N-degree algorithm's
   permutation and path-selection steps could not be obtained verbatim from the
   published specification while this was written, and this is a hash that gets
   signed. Implementing it from a summary is how you produce a plausible wrong
   answer.

   ## Correctness is measured, not asserted

   `test/fixtures/rdfc10/` holds input/expected pairs from the **official W3C test
   suite**, checked in so the suite does not depend on the network. Every test this
   implementation claims to handle is one of those, compared byte for byte against
   W3C's own expected output — not against itself.

   ## Code point order, not UTF-16 order

   §4.4.3 orders serialized quads and hashes in *code point* order. A JVM string
   compare is UTF-16 **code unit** order, and the two differ for supplementary
   characters: a surrogate pair leads with 0xD800-0xDBFF and so sorts before
   U+E000-U+FFFF under code units, and after them under code points. `compare-code-points`
   implements the specified order rather than the convenient one.

   (Note this is the OPPOSITE requirement to RFC 8785 JCS, which explicitly wants
   UTF-16 code unit order for property names. Two specs, two orders, and using one
   library's comparator for the other's job is a silent interoperability bug.)"
  (:require [multiformats.core :as mf]
            [nquads.core :as nq]))

(def canonical-prefix "c14n")

(defn- fail! [code msg data]
  (throw (ex-info msg (assoc data :rdf-canon/error code))))

(defn- utf8-bytes [s]
  #?(:clj (.getBytes ^String s "UTF-8")
     :cljs (.encode (js/TextEncoder.) s)))

(defn- hex [bs]
  (apply str (map (fn [b]
                    (let [i (bit-and b 0xff)]
                      (str (when (< i 16) "0")
                           #?(:clj (Integer/toHexString i)
                              :cljs (.toString i 16)))))
                  (seq bs))))

(defn sha256-hex [s] (hex (mf/sha256 (utf8-bytes s))))

;; ── code point order ─────────────────────────────────────────────────────────

(defn- code-points [s]
  #?(:clj (vec (iterator-seq (.iterator (.codePoints ^String s))))
     :cljs (mapv #(.codePointAt s %)
                 (loop [i 0 acc []]
                   (if (>= i (.-length s)) acc
                       (recur (+ i (if (> (.codePointAt s i) 0xFFFF) 2 1))
                              (conj acc i)))))))

(defn compare-code-points
  "§4.4.3's ordering. NOT `compare`: that is UTF-16 code unit order on the JVM, and
   the two disagree for supplementary characters."
  [a b]
  (let [x (code-points a) y (code-points b)]
    (loop [i 0]
      (cond
        (and (= i (count x)) (= i (count y))) 0
        (= i (count x)) -1
        (= i (count y)) 1
        :else (let [c (compare (nth x i) (nth y i))]
                (if (zero? c) (recur (inc i)) c))))))

;; ── §4.5 identifier issuer ───────────────────────────────────────────────────

(defn issuer
  "An identifier issuer: a prefix, a counter, and the map of what it has issued."
  [prefix]
  {:prefix prefix :counter 0 :issued {} :order []})

(defn issue
  "§4.5.2 Issue Identifier. Returns `[issuer' identifier]`.

   Returns the EXISTING identifier when one was already issued for this label —
   issuing a second would break the bijection the canonical form depends on."
  [{:keys [prefix counter issued order] :as iss} existing]
  (if-let [already (get issued existing)]
    [iss already]
    (let [id (str prefix counter)]
      [{:prefix prefix :counter (inc counter)
        :issued (assoc issued existing id)
        :order (conj order existing)}
       id])))

;; ── dataset helpers ──────────────────────────────────────────────────────────

(defn- blank-labels-of [{:keys [subject predicate object graph]}]
  (into #{} (comp (keep identity)
                  (filter #(= :blank (:type %)))
                  (map :value))
        [subject predicate object graph]))

(defn blank-node-to-quads
  "§4.4.3 step 2. Blank node label -> the quads mentioning it."
  [statements]
  (reduce (fn [acc st]
            (reduce (fn [m label] (update m label (fnil conj []) st))
                    acc (blank-labels-of st)))
          {} statements))

(defn- relabel-term [term f]
  (if (= :blank (:type term)) (nq/blank (f (:value term))) term))

(defn- relabel-statement [st f]
  (reduce (fn [m k] (if (get m k) (update m k relabel-term f) m))
          st [:subject :predicate :object :graph]))

;; ── §4.6 Hash First Degree Quads ─────────────────────────────────────────────

(defn hash-first-degree
  "§4.6.3. Every blank node component is replaced with `a` if it is the reference
   node and `z` otherwise; the serialized quads are code point ordered,
   concatenated, and hashed."
  [bnode->quads reference]
  (let [quads (get bnode->quads reference)]
    (when-not (seq quads)
      (fail! :rdf-canon/unknown-blank-node
             "no quads mention this blank node" {:label reference}))
    (->> quads
         (map #(nq/serialize-statement
                (relabel-statement % (fn [label] (if (= label reference) "a" "z")))))
         (sort compare-code-points)
         (apply str)
         sha256-hex)))

;; ── §4.4.3 the canonicalization algorithm ────────────────────────────────────

(defn canonicalize-dataset
  "Return `{:statements … :issued …}` with blank nodes canonically relabelled.

   Throws `:rdf-canon/n-degree-required` when two or more blank nodes share a
   first-degree hash — see the namespace docstring for why that is a refusal rather
   than a fallback."
  [statements]
  (let [bnode->quads (blank-node-to-quads statements)
        ;; step 3
        hash->labels (reduce (fn [acc label]
                               (update acc (hash-first-degree bnode->quads label)
                                       (fnil conj []) label))
                             {} (keys bnode->quads))
        ;; step 4 — code point ordered by hash, and only the unique ones
        ordered (sort compare-code-points (keys hash->labels))
        shared (filter #(> (count (get hash->labels %)) 1) ordered)]
    (when (seq shared)
      (fail! :rdf-canon/n-degree-required
             (str (count shared) " first-degree hash(es) are shared by more than one "
                  "blank node, which needs the N-degree hashing of §4.4.3 step 5. "
                  "That step is not implemented, and falling back to any other "
                  "ordering would produce a hash that disagrees with every other "
                  "implementation — silently, and only for graphs like this one.")
             {:shared-hashes (vec shared)
              :labels (vec (mapcat #(get hash->labels %) shared))}))
    (let [final (reduce (fn [iss h]
                          (first (issue iss (first (get hash->labels h)))))
                        (issuer canonical-prefix)
                        ordered)
          issued (:issued final)]
      {:statements (mapv #(relabel-statement % (fn [l] (get issued l l))) statements)
       :issued issued})))

(defn canonicalize
  "§4.4.3 step 7: the serialized canonical form of a dataset.

   Takes N-Quads text or a seq of statements; returns canonical N-Quads — blank
   nodes relabelled `c14n0`, `c14n1`, … and the statements code point ordered."
  [input]
  (let [statements (if (string? input) (nq/parse input) (vec input))
        {:keys [statements]} (canonicalize-dataset statements)]
    (->> statements
         (map nq/serialize-statement)
         (sort compare-code-points)
         (apply str))))

(defn canonical-hash
  "The hash of the canonical form — what a `-rdfc-` cryptosuite signs over."
  [input]
  (sha256-hex (canonicalize input)))
