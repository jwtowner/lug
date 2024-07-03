lug
[![Build Status](https://github.com/jwtowner/lug/actions/workflows/c-cpp.yml/badge.svg)](https://github.com/jwtowner/lug/actions/workflows/c-cpp.yml)
[![License](https://img.shields.io/packagist/l/doctrine/orm.svg)](https://github.com/jwtowner/lug/blob/master/LICENSE.md)
===
A C++ embedded domain specific language for expressing parsers as extended [parsing expression grammars (PEGs)](https://en.wikipedia.org/wiki/Parsing_expression_grammar)

![lug](https://github.com/jwtowner/lug/raw/master/doc/lug_logo_large.png)

Features
---
- Natural syntax resembling external parser generator languages, with support for attributes and semantic actions.
- Ability to handle context-sensitive grammars with symbol tables, conditions and syntactic predicates.
- Generated parsers are compiled to special-purpose bytecode and executed in a virtual parsing machine.
- Clear separation of syntactic and lexical rules, with the ability to customize implicit whitespace skipping.
- Support for direct and indirect left recursion, with precedence levels to disambiguate subexpressions with mixed left/right recursion.
- Full support for UTF-8 text parsing, including Level 1 and partial Level 2 compliance with the UTS #18 Unicode Regular Expressions technical standard.
- Automatic tracking of line and column numbers, with customizable tab width and alignment.
- Header-only library utilizing C++17 language and library features.
- Relatively small with the goal of keeping total line count across all header files under 5000 lines of terse code.

It is based on research introduced in the following papers:

> Bryan Ford, [Parsing expression grammars: a recognition-based syntactic foundation](https://doi.org/10.1145/982962.964011), Proceedings of the 31st ACM SIGPLAN-SIGACT symposium on Principles of Programming Languages, p.111-122, January 2004

> Sérgio Medeiros et. al, [A parsing machine for PEGs](https://doi.org/10.1145/1408681.1408683), Proceedings of the 2008 symposium on Dynamic Languages, p.1-12, July 2008

> Kota Mizushima et. al, [Packrat parsers can handle practical grammars in mostly constant space](https://doi.org/10.1145/1806672.1806679), Proceedings of the 9th ACM SIGPLAN-SIGSOFT workshop on Program analysis for software tools and engineering, p.29-36, June 2010

> Sérgio Medeiros et. al, [Left recursion in Parsing Expression Grammars](https://doi.org/10.1016/j.scico.2014.01.013), Science of Computer Programming, v.96 n.P2, p.177-190, December 2014

> Leonardo Reis et. al, [The formalization and implementation of Adaptable Parsing Expression Grammars](https://doi.org/10.1016/j.scico.2014.02.020), Science of Computer Programming, v.96 n.P2, p.191-210, December 2014

> Tetsuro Matsumura, Kimio Kuramitsu, [A Declarative Extension of Parsing Expression Grammars for Recognizing Most Programming Languages](https://doi.org/10.2197/ipsjjip.24.256), Journal of Information Processing, v.24 i.2, p.256-264, November 2015

> Sérgio Medeiros et. al, [A parsing machine for parsing expression grammars with labeled failures](https://doi.org/10.1145/2851613.2851750), Proceedings of the 31st Annual ACM symposium on Applied Computing, p.1960-1967, April 2016

Building
---
As a header-only library, lug itself does not require any build process.
To use lug, make sure to include the `lug` header directory in your project's include path.
Once that is done, you are ready to start using lug in your code.
To build the sample programs and unit tests both [CMake](https://cmake.org/) and [make](https://pubs.opengroup.org/onlinepubs/9699919799/utilities/make.html) are supported.

As a baseline, the following compiler versions are known to work with lug.

| Compiler | Language Mode |
| --- | --- |
| Clang 14.0.0 (March 2022) | -std=c++17 or -std=gnu++17 |
| Clang 18.1.0 (March 2024) | -std=c++17 or -std=gnu++17 |
| GCC 9.5 (May 2022) | -std=c++17 or -std=gnu++17 |
| GCC 10.5 (July 2023) | -std=c++17 or -std=gnu++17 |
| GCC 11.4 (May 2023) | -std=c++17 or -std=gnu++17 |
| GCC 12.4 (June 2024) | -std=c++17 or -std=gnu++17 |
| GCC 13.3 (May 2024) | -std=c++17 or -std=gnu++17 |
| Microsoft Visual C++ 2017 15.9 (November 2018) | Platform Toolset: Visual Studio 2017 Toolset (v141), Language Standard: ISO C++17 Standard (/std:c++17) |
| Microsoft Visual C++ 2019 16.11 (August 2021) | Platform Toolset: Visual Studio 2019 Toolset (v142), Language Standard: ISO C++17 Standard (/std:c++17) |
| Microsoft Visual C++ 2022 17.10 (May 2024) | Platform Toolset: Visual Studio 2022 Toolset (v143), Language Standard: ISO C++17 Standard (/std:c++17) |

Syntax Reference
---

| Operator | Syntax | Description |
| --- | --- | --- |
| Ordered Choice | `e1 \| e2` | Attempts to first match expression *e1*, and if that fails backtracks then attempts to match *e2*. |
| Sequence | `e1 > e2` | Matches both expressions *e1* followed by *e2* in sequence. |
| List | `e1 >> e2` | Repetition matching of a sequence of one or more *e1* expressions delimited by *e2*. Shorthand for `e1 > *(e2 > e1)`. |
| Zero-or-More | `*e` | Repetition matching of expression *e* zero, one or more times. |
| One-or-More | `+e` | Repetition matching of expression *e* one or more times. |
| Optional | `~e` | Matches expression *e* zero or one times. |
| Positive Lookahead | `&e` | Matches without consuming input if expression *e* succeeds to match the input. |
| Negative Lookahead | `!e` | Matches without consuming input if expression *e* fails to match the input. |
| Cut Before | `--e` | Issues a cut instruction before the expression *e*. |
| Cut After | `e--` | Issues a cut instruction after the expression *e*. |
| Action Scheduling | `e < a` | Schedules a semantic action *a* to be evaluated if expression *e* successfully matches the input. |
| Attribute Binding | `v % e` | Assigns the return value of the last evaluated semantic action within the expression *e* to the variable *v*. |
| Syntactic Capture | `capture(v)⁠[e]` | Captures the text matching the subexpression *e* into variable *v*. |

| Control | Description |
| --- | --- |
| `cased⁠[e]` | Case sensitive matching for the subexpression *e* (the default) |
| `caseless⁠[e]` | Case insensitive matching for subexpression *e* |
| `skip⁠[e]` | Turns on all whitespace skipping for subexpression *e* (the default) |
| `noskip⁠[e]` | Turns off all whitespace skipping for subexpression *e*, including preceeding whitespace |
| `lexeme⁠[e]` | Treats subexpression *e* as a lexical token with no internal whitespace skipping |
| `on(C)⁠[e]` | Sets the condition *C* to true for the scope of subexpression *e* |
| `off(C)⁠[e]` | Sets the condition *C* to false for the scope of subexpression *e* (the default) |
| `symbol(S)⁠[e]` | Pushes a symbol definition for symbol *S* with value equal to the captured input matching subexpression *e* |
| `block⁠[e]` | Creates a scope block for subexpression *e* where all new symbols defined in *e* are local to it and all external symbols defined outside of the block are also available for reference within *e* |
| `local⁠[e]` | Creates a local scope block for subexpression *e* where all new symbols defined in *e* are local to it and there are no external symbol definitions available for reference |
| `local(S)⁠[e]` | Creates a local scope block for subexpression *e* where all new symbols defined in *e* are local to it and all external symbols defined outside of the block are also available for reference within *e*, except for the symbol named *S* |

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
| `cntrl` | Matches any control character |
| `digit` | Matches any decimal digit |
| `graph` | Matches any graphical character |
| `lower` | Matches any lowercase alphabetical character |
| `print` | Matches any printable character |
| `punct` | Matches any punctuation character |
| `space` | Matches any whitespace character |
| `upper` | Matches any uppercase alphabetical character |
| `xdigit` | Matches any hexadecimal digit  |
| `when⁠(C)` | Matches if the condition named *C* is *true*, without consuming input |
| `unless⁠(C)` | Matches if the condition named *C* is *false*, without consuming input |
| `exists⁠(S)` | Matches if there is a definition for symbol *S* in the current scope |
| `missing⁠(S)` | Matches if there is no definition for symbol *S* in the current scope |
| `match⁠(S)` | Matches the last definition for symbol named *S* |
| `match_any⁠(S)` | Matches against any prior definition for symbol named *S* |
| `match_all⁠(S)` | Matches against all prior definitions for symbol named *S*, in sequence from least to most recent |
| `match_front⁠(S,N=0)` | Matches against the *N*-th least recent definition for symbol named *S* |
| `match_back⁠(S,N=0)` | Matches against the *N*-th most recent definition for symbol named *S* |

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