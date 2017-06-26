lug
===
An embedded domain specific language in C++ for expressing parsers as extended Parsing Expression Grammars (PEGs)

- Natural syntax more akin to external parser generator languages, unlike many other C++ parsing toolkits
- Left recursion with precedence levels to disambiguate subexpressions with mixed left/right recursion
- Traditional PEG syntax has been extended to support attribute grammars
- Generated parsers are compiled down to special-purpose bytecode and executed in a virtual parsing machine
- Uses expression template functors for evaluating and optimizing grammar rules at compile time
- UTF-8 text parsing with support for matching Unicode character classes
- Header only library using C++17 language and library features
- Relatively small, intended to remain under 1000 lines of (very terse) code

It is based on research introduced in the following papers:

> Bryan Ford, [Parsing expression grammars: a recognition-based syntactic foundation](https://doi.org/10.1145/982962.964011), Proceedings of the 31st ACM SIGPLAN-SIGACT symposium on Principles of Programming Languages, p.111-122, January 2004

> Sérgio Medeiros et. al, [A parsing machine for PEGs](https://doi.org/10.1145/1408681.1408683), Proceedings of the 2008 symposium on Dynamic Languages, p.1-12, July 2008

> Sérgio Medeiros et. al, [Left recursion in Parsing Expression Grammars](https://doi.org/10.1016/j.scico.2014.01.013), Science of Computer Programming, v.96 n.P2, p.177-190, December 2014

> Leonardo Reis et. al, [The formalization and implementation of Adaptable Parsing Expression Grammars](https://doi.org/10.1016/j.scico.2014.02.020), Science of Computer Programming, v.96 n.P2, p.191-210, December 2014

> Sérgio Medeiros et. al, [A parsing machine for parsing expression grammars with labeled failures](https://doi.org/10.1145/2851613.2851750), Proceedings of the 31st Annual ACM symposium on Applied Computing, p.1960-1967, April 2016

TODO
---
- parser error recovery (WIP)
- cut operator should ensure back-tracking is no longer possible, possibly investigate using progressive tabling and dynamic analysis
- investigate binding only types and passing in attribute variables to lambdas as arguments
- optimization: better detection of left-recursive rule calls
- optimization: do not create attribute variable frames if a rule does not use the attribute binding operator
- optimization: tail recursion
- optimization: inlining
- optimization: additional instructions
- more samples, testing, and bug fixing
- increase compiler warning level and fix issues
- add documentation