lug
[![Build Status](https://github.com/jwtowner/lug/actions/workflows/c-cpp.yml/badge.svg)](https://github.com/jwtowner/lug/actions/workflows/c-cpp.yml)
[![License](https://img.shields.io/packagist/l/doctrine/orm.svg)](https://github.com/jwtowner/lug/blob/master/LICENSE.md)
===
A C++ embedded domain specific language for expressing parsers as extended [parsing expression grammars (PEGs)](https://en.wikipedia.org/wiki/Parsing_expression_grammar)

![lug](https://github.com/jwtowner/lug/raw/master/doc/lug_logo_large.png)

Features
---
- Natural syntax similar to other external parser generator languages
- Clear separation of syntactic and lexical rules, with the ability to customize implicit whitespace skipping
- Support for direct and indirect left recursion, with precedence levels to disambiguate subexpressions with mixed left/right recursion
- Extended PEG syntax to include attribute grammars and semantic actions
- Ability to handle context-sensitive grammars with symbol tables, conditions, and syntactic predicates
- Cut operator to commit to the currently matched parse prefix and prune all backtrack entries
- Deferred evaluation of semantic actions, ensuring actions only execute on successful branches and valid input
- Generated parsers are compiled to special-purpose bytecode and executed in a virtual parsing machine
- Full support for UTF-8 text parsing, including Level 1 and partial Level 2 compliance with the UTS #18 Unicode Regular Expressions technical standard
- Automatic tracking of line and column numbers, with customizable tab width and alignment
- Implementation of rules in the domain-specific language using expression templates and function objects
- Header-only library utilizing C++17 language and library features
- Relatively small codebase, with the goal of keeping the parser core under 2000 lines of concise code

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
As a header-only library, lug itself does not require any build process.
To use lug, make sure to include the `lug` header directory in your project's include path.
Once that is done, you are ready to start using lug in your code.

As a baseline, the following compiler versions are known to work with lug.

| Compiler | Language Mode |
| --- | --- |
| Clang 14.0.0 (March 2022) | -std=c++17 or -std=gnu++17 |
| GCC 7.1.0 (May 2017) | -std=c++17 or -std=gnu++17 |
| Microsoft Visual C++ 2017 15.5 (December 2017) | Platform Toolset: Visual Studio 2017 Toolset (v141), Language Standard: ISO C++17 Standard (/std:c++17) |

To build the sample programs and unit tests, a makefile is provided for Linux and BSD platforms and a Visual Studio solution is available for use on Windows.

Syntax Reference
---

<style>
  .center-text {
    text-align: center;
  }
  .code-style {
    font-family: ui-monospace,SFMono-Regular,SF Mono,Menlo,Consolas,Liberation Mono,monospace;
    background-color: #454545;
    border-radius: 3px;
    padding: 2px 4px;
    color: lightgrey;
  }
</style>
<table style="width:100%">
  <tr>
    <th style="width:15%">Operator</th>
    <th class="center-text" style="width:20%">Syntax</th>
    <th style="width:65%">Description</th>
  </tr>
  <tr>
    <td>Sequence</td>
    <td class="center-text"><span class="code-style">e1 > e2</span></td>
    <td>Matches both expressions <i>e1</i> followed by <i>e2</i> in sequence</td>
  </tr>
  <tr>
    <td>Ordered Choice</td>
    <td class="center-text"><span class="code-style">e1 | e2</span></td>
    <td>Attempts to first match expression <i>e1</i>, and if that fails backtracks then attempts to match <i>e2</i></td>
  </tr>
  <tr>
    <td>Zero-or-More</td>
    <td class="center-text"><span class="code-style">*e</span></td>
    <td>Reptition matching of expression <i>e</i> zero, one or more times</td>
  </tr>
  <tr>
    <td>One-or-More</td>
    <td class="center-text"><span class="code-style">+e</span></td>
    <td>Reptition matching of expression <i>e</i> one or more times</td>
  </tr>
  <tr>
    <td>Optional</td>
    <td class="center-text"><span class="code-style">~e</span></td>
    <td>Matching of expression <i>e</i> zero or one times</td>
  </tr>
  <tr>
    <td>Positive Lookahead</td>
    <td class="center-text"><span class="code-style">&e</span></td>
    <td>Matches without consuming input if expression <i>e</i> succeeds to match the input</td>
  </tr>
  <tr>
    <td>Cut Before</td>
    <td class="center-text"><span class="code-style">--e</span></td>
    <td>Issues a cut instruction before the expression <i>e</i></td>
  </tr>
  <tr>
    <td>Cut After</td>
    <td class="center-text"><span class="code-style">e--</span></td>
    <td>Issues a cut instruction after the expression <i>e</i></td>
  </tr>
  <tr>
    <td>Action Scheduling</td>
    <td class="center-text"><span class="code-style">e < a</span></td>
    <td>Schedules a semantic action <i>a</i> to be evaluated if expression <i>e</i> successfully matches the input</td>
  </tr>
  <tr>
    <td>Attribute Binding</td>
    <td class="center-text"><span class="code-style">v % e</span></td>
    <td>Assigns the return value of the last evaluated semantic action within the expression <i>e</i> to the variable <i>v</i></td>
  </tr>
  <tr>
    <td>Syntactic Capture</td>
    <td class="center-text"><span class="code-style">capture(v)[e]</span></td>
    <td>Captures the text matching the subexpression <i>e</i> into variable <i>v</i></td>
  </tr>
</table>

| Control | Description |
| --- | --- |
| `cased[e]` | Case sensitive matching for the subexpression *e* (the default) |
| `caseless[e]` | Case insensitive matching for subexpression *e* |
| `skip[e]` | Turns on all whitespace skipping for subexpression *e* (the default) |
| `noskip[e]` | Turns off all whitespace skipping for subexpression *e*, including preceeding whitespace |
| `lexeme[e]` | Treats subexpression *e* as a lexical token with no internal whitespace skipping |
| `on(C)[e]` | Sets the condition *C* to true for the scope of subexpression *e* |
| `off(C)[e]` | Sets the condition *C* to false for the scope of subexpression *e* (the default) |
| `symbol(S)[e]` | Pushes a symbol definition for symbol *S* with value equal to the captured input matching subexpression *e* |
| `block[e]` | Creates a scope block for subexpression *e* where all new symbols defined in *e* are local to it and all external symbols defined outside of the block are also available for reference within *e* |
| `local[e]` | Creates a local scope block for subexpression *e* where all new symbols defined in *e* are local to it and there are no external symbol definitions available for reference |
| `local(S)[e]` | Creates a local scope block for subexpression *e* where all new symbols defined in *e* are local to it and all external symbols defined outside of the block are also available for reference within *e*, except for the symbol named *S* |

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