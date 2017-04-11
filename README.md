lug
===
Lug is a compact embedded DSL for expressing parsers as Parsing Expression Grammars (PEGs) in C++.

TODO
---
- settle on a better memory management scheme for the parsing rules during grammar construction
- implement variable scoping for the attribute extraction operator, bound to a rule expression
- convert regular string literals directly into terminals, character classes, etc.
- experiment with reimplementing as a virtual parsing machine via *A Parsing Machine for PEGs, Medeiros et al.*
- more samples and testing
- increase compiler warning level and fix issues
- add documentation
