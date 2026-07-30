# kotoba-lang/org-w3-rdf-canon

**[RDF Dataset Canonicalization (RDFC-1.0)](https://www.w3.org/TR/rdf-canon/), portable `.cljc`.**

Second layer of the `-rdfc-` cryptosuite stack. `org-w3-nquads` fixes the bytes for a
term; this fixes which **blank node labels** those bytes carry, so two parties holding
the same graph produce the same hash even though blank node labels are arbitrary.

## What is implemented, and what refuses rather than guesses

The simple path is complete: state construction, first-degree hashing, and issuing
canonical identifiers to blank nodes whose first-degree hash is unique (§4.4.3
steps 1–4, 6–7).

**Step 5 — the N-degree hashing that resolves blank nodes *sharing* a first-degree
hash — is not implemented, and `canonicalize` throws `:rdf-canon/n-degree-required`
when it is reached.**

That refusal is the design decision. A canonicalizer that fell back to some other
ordering would return a hash that looks perfectly normal and **disagrees with every
other implementation — silently, and only for graphs with interconnected blank
nodes**, which is precisely the non-trivial case. Throwing means a caller learns the
limit when it matters, rather than discovering it later as an unverifiable signature.

Why not implemented: the N-degree permutation and path-selection steps could not be
obtained verbatim from the published spec while this was written, and this is a hash
that gets **signed**. Implementing it from a summary is how you produce a plausible
wrong answer.

## Correctness is measured, not asserted

`test/fixtures/rdfc10/` holds input/expected pairs from the **official W3C test
suite**, committed so the suite needs no network. Current standing:

```
official RDFC-1.0 vectors: 14 matched, 3 deferred to N-degree, 0 other, of 17
```

The assertion that matters is that **nothing produces a wrong canonical form** — a
mismatch would be worse than a refusal. A canonicalizer that only agreed with itself
would be worthless; the whole point is that two independent parties produce the same
bytes.

## Code point order, not UTF-16 order

§4.4.3 orders in **code point** order. A JVM string compare is UTF-16 **code unit**
order, and they differ for supplementary characters: a surrogate pair leads with
0xD800–0xDBFF, so it sorts *before* U+E000 under code units and *after* it under code
points.

This is the **opposite** requirement to RFC 8785 JCS, which explicitly wants UTF-16
code unit order for property names. Two specs, two orders — using one library's
comparator for the other's job is a silent interoperability bug, so
`compare-code-points` implements the specified order rather than the convenient one.

## Test

```bash
clojure -M:dev:test
clojure -M:lint
```

## License

MIT. See `LICENSE`.
