lug
===
An embedded domain specific language in C++ for expressing parsers as extended Parsing Expression Grammars (PEGs)

Key features:

- Header only library written using modern C++17 language and library features
- Features a natural syntax more akin to an external parser generator language, unlike other C++ parsing toolkits
- Generated parsers are compiled down to a special-purpose bytecode that runs in a compact parsing machine
- Uses fexpr-like template expression functors for evaluating and optimizing grammar rule expression trees at compile time
- Traditional PEG syntax has been extended to support attribute grammars
- Written to be extremely small, is currently under 1000 lines of (very terse) code (this may be relaxed in the future)

It is based off of the research introduced in the following papers:

> [Bryan Ford, Parsing expression grammars: a recognition-based syntactic foundation, Proceedings of the 31st ACM SIGPLAN-SIGACT symposium on Principles of programming languages, p.111-122, January 2004](https://doi.org/10.1145/583852.581483)
> [Sérgio Medeiros et. al, A parsing machine for PEGs, Proceedings of the 2008 symposium on Dynamic languages, p.1-12, July 2008](https://doi.org/10.1145/1408681.1408683)
> [Sérgio Medeiros et. al, Left recursion in Parsing Expression Grammars, Science of Computer Programming, v.96 n.P2, p.177-190, December 2014](https://doi.org/10.1016/j.scico.2014.01.013)
> [Leonardo Reis et. al, The formalization and implementation of Adaptable Parsing Expression Grammars, Science of Computer Programming, v.96 n.P2, p.191-210, December 2014](https://doi.org/10.1016/j.scico.2014.02.020)
> [Sérgio Medeiros et. al, A parsing machine for parsing expression grammars with labeled failures, Proceedings of the 31st Annual ACM Symposium on Applied Computing, p.1960-1967, April 2016](https://doi.org/10.1145/2851613.2851750)

Todo
---
- left recursion (WIP)
- parser error recovery (WIP)
- intermediate accept should ensure back-tracking is no longer possible, possibly investigate using progressive tabling and dynamic analysis
- tail recursion optimization of rules
- rule inlining optimization
- additional instructions for optimizations
- more samples, testing, and bug fixing
- increase compiler warning level and fix issues
- add documentation
