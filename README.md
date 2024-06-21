lug
[![Build Status](https://github.com/jwtowner/lug/actions/workflows/c-cpp.yml/badge.svg)](https://github.com/jwtowner/lug/actions/workflows/c-cpp.yml)
[![License](https://img.shields.io/packagist/l/doctrine/orm.svg)](https://github.com/jwtowner/lug/blob/master/LICENSE.md)
===
A C++ embedded domain specific language for expressing parsers as extended [parsing expression grammars (PEGs)](https://en.wikipedia.org/wiki/Parsing_expression_grammar)

![lug](https://github.com/jwtowner/lug/raw/master/doc/lug_logo_large.png)

Features
---
- Natural syntax more akin to external parser generator languages
- Separation of syntatic and lexical rules, with customizable implicit whitespace skipping
- Direct and indirect left recursion with precedence levels to disambiguate subexpressions with mixed left/right recursion
- Traditional PEG syntax has been extended to support attribute grammars and semantic actions
- Support for most context-sensitive grammars with symbol tables, conditions and syntactic predicates
- Cut operator to commit to currently matched parse prefix and prune all backtrack entries
- Deferred evaluation of semantic actions, ensuring actions do not execute on failed branches or invalid input
- Generated parsers are compiled to special-purpose bytecode and executed in a virtual parsing machine
- UTF-8 text parsing with complete Level 1 and partial Level 2 support of the UTS #18 Unicode Regular Expressions technical standard
- Automatic line and column tracking with customizable tab width and alignment
- Uses expression template functors to implement the rules of the domain specific language
- Header only library using C++17 language and library features
- Relatively small with the intent of parser core to remain under 2000 lines of terse code

It is based on research introduced in the following papers:

> Bryan Ford, [Parsing expression grammars: a recognition-based syntactic foundation](https://doi.org/10.1145/982962.964011), Proceedings of the 31st ACM SIGPLAN-SIGACT symposium on Principles of Programming Languages, p.111-122, January 2004

> Sérgio Medeiros et. al, [A parsing machine for PEGs](https://doi.org/10.1145/1408681.1408683), Proceedings of the 2008 symposium on Dynamic Languages, p.1-12, July 2008

> Kota Mizushima et. al, [Packrat parsers can handle practical grammars in mostly constant space](https://doi.org/10.1145/1806672.1806679), Proceedings of the 9th ACM SIGPLAN-SIGSOFT workshop on Program analysis for software tools and engineering, p.29-36, June 2010

> Sérgio Medeiros et. al, [Left recursion in Parsing Expression Grammars](https://doi.org/10.1016/j.scico.2014.01.013), Science of Computer Programming, v.96 n.P2, p.177-190, December 2014

> Leonardo Reis et. al, [The formalization and implementation of Adaptable Parsing Expression Grammars](https://doi.org/10.1016/j.scico.2014.02.020), Science of Computer Programming, v.96 n.P2, p.191-210, December 2014

> Tetsuro Matsumura, Kimio Kuramitsu, [A Declarative Extension of Parsing Expression Grammars for Recognizing Most Programming Languages](https://doi.org/10.2197/ipsjjip.24.256), Journal of Information Processing, v.24 i.2 p.256-264, November 2015

> Sérgio Medeiros et. al, [A parsing machine for parsing expression grammars with labeled failures](https://doi.org/10.1145/2851613.2851750), Proceedings of the 31st Annual ACM symposium on Applied Computing, p.1960-1967, April 2016

Building
---
As a header only library, lug itself does not need to be built.
Simply ensure the `lug` header directory is in your include path and you're good to go.

As a baseline, the following compiler versions are known to work with lug.

| Compiler | Language Mode |
| --- | --- |
| Clang 14.0.0 (March 2022) | -std=c++17 or -std=gnu++17 |
| GCC 7.1.0 (May 2017) | -std=c++17 or -std=gnu++17 |
| Microsoft Visual C++ 2017 15.5 (December 2017) | Platform Toolset: Visual Studio 2017 Toolset (v141), Language Standard: ISO C++17 Standard (/std:c++17) |

To build the sample programs and unit tests, a makefile is provided for Linux and BSD platforms and a Visual Studio solution is available for use on Windows.

Syntax Reference
---

| Operator | Syntax | Description|
| --- | --- | --- |
| Sequence | *e1* > *e2* | Matches both expressions *e1* followed by *e2* in sequence |
| Ordered Choice | *e1* \| *e2* | Attempts to first match expression *e1*, and if that fails backtracks then attempts to match *e2* |
| Zero-or-More | \**e* | Reptition matching of expression *e* zero, one or more times |
| One-or-More | +*e* | Reptition matching of expression *e* one or more times |
| Optional | ~*e* | Matching of expression *e* zero or one times |
| Positive Lookahead | &*e* | Matches without consuming input if expression *e* succeeds to match the input |
| Negative Lookahead | !*e* | Matches without consuming input if expression *e* fails to match the input |
| Cut Before | --*e* | Issues a cut instruction before the expression *e* |
| Cut After | *e*-- | Issues a cut instruction after the expression *e* |
| Action Scheduling | *e* < *a* | Schedules a semantic action *a* to be evaluated if expression *e* succesfully matches the input |
| Attribute Binding | *v* % *e* | Binds the return value of the last evaluated semantic action within the expression *e* to the attribute *v* |

| Control | Description |
| --- | --- |
| `cased[e]` | Case sensitive matching for the subexpression *e* (the default) |
| `caseless[e]` | Case insensitive matching for subexpression *e* |
| `skip[e]` | Turns on all whitespace skipping for subexpression *e* (the default) |
| `noskip[e]` | Turns off all whitespace skipping for subexpression *e*, including preceeding whitespace |
| `lexeme[e]` | Treats subexpression *e* as a lexical token with no internal whitespace skipping |
| `capture(V)[e]` | Syntactic capture of the matching subexpression *e* into variable *V* |
| `on(C)[e]` | Sets the condition *C* to true for the scope of subexpression *e* |
| `off(C)[e]` | Sets the condition *C* to false for the scope of subexpression *e* (the default) |
| `symbol(S)[e]` | Pushes a symbol definition for symbol *S* that is equal to the captured input that matches subexpression *e* |
| `block[e]` | Creates a scope block for subexpression *e* where all new symbols defined in *e* are local to it and all external symbols defined outside of the block are also available for reference within *e* |
| `local[e]` | Creates a local scope block for subexpression *e* where all new symbols defined in *e* are local to it and there are no external symbol definitions available for reference |
| `local(S)[e]` | Creates a local scope block for subexpression *e* where all new symbols defined in *e* are local to it and all external symbols defined outside of the block are also available for reference within *e*,
except for the symbol named *S* |

| Terminal | Description |
| --- | --- |
| `nop` | No operation, does not emit any instructions |
| `eps` | Matches the empty string |
| `eoi` | Matches the end of the input sequence |
| `eol` | Matches a Unicode line-ending |
| `cut` | Emits a cut operation into the stream of semantic actions |
| `chr(c)` | Matches the UTF-8, UTF-16, or UTF-32 character *c* |
| `chr(c1, c2)` | Matches characters in the UTF-8, UTF-16, or UTF-32 interval \[*c1*-*c2*\] |
| `str(s)` | Matches the sequence of characters in the string *s* |
| `bre(s)` | POSIX Basic Regular Expression (BRE) |
| `any` | Matches any single character |
| `any(flags)` | Matches a character exhibiting any of the character properties |
| `all(flags)` | Matches a character with all of the character properties |
| `none(flags)` | Matches a character with none of the character properties |
| `alpha` | Matches any alphabetical character |
| `alnum` | Matches any alphabetical character or numerical digit |
| `blank` | Matches any space or tab character |
| `digit` | Matches any decimal digit |
| `graph` | Matches any graphical character |
| `lower` | Matches any lowercase alphabetical character |
| `print` | Matches any printable character |
| `punct` | Matches any punctuation character |
| `space` | Matches any whitespace character |
| `upper` | Matches any uppercase alphabetical character |
| `xdigit` | Matches any hexadecimal digit  |
| `when(C)` | Matches if the condition named *C* is *true*, without consuming input |
| `unless(C)` | Matches if the condition named *C* is *false*, without consuming input |
| `exists(S)` | Matches if there is a definition for symbol *S* in the current scope |
| `missing(S)` | Matches if there is no definition for symbol *S* in the current scope |
| `match(S)` | Matches the last definition for symbol named *S* |
| `match_any(S)` | Matches against any prior definition for symbol named *S* |
| `match_all(S)` | Matches against all prior definitions for symbol named *S*, in sequence from least to most recent |
| `match_front(S,N=0)` | Matches against the *N*-th least recent definition for symbol named *S* |
| `match_back(S,N=0)` | Matches against the *N*-th most recent definition for symbol named *S* |

| Literal | Name | Description |
| --- | --- | --- |
| `_cx` | Character Expression | Matches the UTF-8, UTF-16, or UTF-32 character literal |
| `_sx` | String Expression | Matches the sequence of characters in a string literal |
| `_rx` | Regular Expression | POSIX Basic Regular Expression (BRE) |
| `_icx` | Case Insensitive Character Expression | Same as `_cx` but case insensitive |
| `_isx` | Case Insensitive String Expression | Same as `_sx` but case insensitive |
| `_irx` | Case Insensitive Regular Expression | Same as `_rx` but case insensitive |
| `_scx` | Case Sensitive Character Expression | Same as `_cx` but case sensitive |
| `_ssx` | Case Sensitive String Expression | Same as `_sx` but case sensitive |
| `_srx` | Case Sensitive Regular Expression | Same as `_rx` but case sensitive |

TODO
---
- parser error recovery
- add an interactive processing mode flag to input sources?
- handle exceptions thrown from semantic actions in semantics::accept?
- feature: Adams-Nestra grammars and whitespace alignment
- feature: syntax to specify number range of allowed iteration
- optimization: tail recursion
- optimization: reduce number of false-positive left-recursive calls even further by lazily evaluating rule mandate
- optimization: additional instructions (test_char, test_any, test_range, test_class)
- more samples, testing, and bug fixing
- increase compiler warning level and fix any issues
- documentation
