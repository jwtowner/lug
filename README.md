lug
[![Build Status](https://github.com/jwtowner/lug/actions/workflows/c-cpp.yml/badge.svg)](https://github.com/jwtowner/lug/actions/workflows/c-cpp.yml)
[![Clang-Analyze](https://github.com/jwtowner/lug/actions/workflows/clang-analyze.yml/badge.svg)](https://github.com/jwtowner/lug/actions/workflows/clang-analyze.yml)
[![Clang-Tidy](https://github.com/jwtowner/lug/actions/workflows/clang-tidy.yml/badge.svg)](https://github.com/jwtowner/lug/actions/workflows/clang-tidy.yml)
[![CodeQL](https://github.com/jwtowner/lug/actions/workflows/dynamic/github-code-scanning/codeql/badge.svg)](https://github.com/jwtowner/lug/actions/workflows/dynamic/github-code-scanning/codeql)
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
- Error handling and recovery with labeled failures, recovery rules and error handlers.
- Automatic tracking of line and column numbers, with customizable tab width and alignment.
- Header-only library utilizing C++17 language and library features. Forward compatible with C++20 and C++23.
- Relatively small with the goal of keeping total line count across all header files under 6000 lines of terse code.

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
As a self-contained header-only library, lug itself does not require any build process.
To use lug, make sure to include the `lug` header directory in your project's include path.
Once that is done, you are ready to start using lug in your code.
To build the sample programs and unit tests both [CMake](https://cmake.org/) and [make](https://pubs.opengroup.org/onlinepubs/9699919799/utilities/make.html) are supported.

As a baseline, the following compiler versions are known to work with lug.

| Compiler | Minimum Language Mode |
| --- | --- |
| Clang 14.0.0 (March 2022) or later | -std=c++17 or -std=gnu++17 |
| GCC 9.5 (May 2022) or later | -std=c++17 or -std=gnu++17 |
| Microsoft Visual C++ 2019 16.11 (August 2021) or later | Platform Toolset: Visual Studio 2019 Toolset (v142), Language Standard: ISO C++17 Standard (/std:c++17) |

Demonstration
---
The following example demonstrates an arithmetic expression evaluator supporting addition and multiplication.

`demo.cpp`
```cpp
// Include the lug library header file
#include <lug/lug.hpp>

// Needed for std::cout
#include <iostream>

int main()
{
    // Import the namespace containing the embedded DSL operators and types
    using namespace lug::language;

    // Define attribute variables for the recursive rules
    int lhs = 0;
    int rhs = 0;

    // Define a lexical rule that matches one or more digits and converts them to an integer
    auto Number = lexeme[+digit] <[](syntax s){ return std::stoi(std::string{s.str()}); };

    // Forward declaration for recursive rules
    rule Expr;

    // Define a rule that matches a number or a parenthesized expression
    rule Factor = Number | ('(' > Expr > ')');

    // Define a rule that multiplies the factors
    rule Term = lhs%Factor > *('*' > rhs%Factor <[&]{ lhs *= rhs; }) <[&]{ return lhs; };

    // Define a rule that adds the terms
    Expr = lhs%Term > *('+' > rhs%Term <[&]{ lhs += rhs; }) <[&]{ return lhs; };

    // Create grammar that matches an arithmetic expression followed by end-of-input
    auto grammar = start(Expr > eoi);

    // Sample input string to parse
    std::string input = "2 * (3 + 4)";

    // Parse and evaluate the sample input
    lug::environment env;
    if (!lug::parse(input, grammar, env)) {
        std::cout << "parse failed\n";
        return 1;
    }

    // Pop the result from the environment and display it to the console
    int result = env.pop_attribute<int>();
    std::cout << input << " = " << result << "\n"; // Outputs: 2 * (3 + 4) = 14
    return 0;
}
```

To compile the demonstration with GCC, save the code above to a file named `demo.cpp` and use the following command,
making sure to substitute `<path-to-lug>` with the location of `lug` on your filesystem:

`g++ -std=c++17 -I<path-to-lug> -o demo demo.cpp`

Then run the demonstration executable with the following command:

`./demo`

You should see the output:

`2 * (3 + 4) = 14`

In summary, the above example demonstrates:
- Lexical rules with semantic actions to convert matched text into values.
- Recursive grammar rules for handling nested expressions.
- Operator precedence through hierarchical rule structure (multiplication before addition).
- Attribute capture and propagation for expression evaluation.
- Environment management for storing and retrieving parsed results.

Quick Reference
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
| Error Handler | `e ^= [⁠]⁠(⁠error_context&⁠)⁠{⁠}` | Associates the error handler callable with expression *e*. |
| Error Response | `e ^ error_response` | Returns the specified `error_response` enumeration value for a recovery rule expression *e*. |
| Recover With | `e[recover_with(r)]` | Installs rule *r* as the default for error recovery for failures in expression *e*. |
| Expects | `e[failure(f)]` | Expects that expression *e* will successfully match, otherwise raises the labeled failure *f*. |
| Expects | `e[failure(f,r)]` | Expects that expression *e* will successfully match, otherwise raises the labeled failure *f* and recovers with rule *r*. |

| Control Directive | Description |
| --- | --- |
| `capture(v)⁠[e]` | Syntactic capture of the text matching the subexpression *e* into variable *v*. |
| `cased⁠[e]` | Case sensitive matching for the subexpression *e* (the default). |
| `caseless⁠[e]` | Case insensitive matching for subexpression *e*. |
| `skip⁠[e]` | Turns on all whitespace skipping for subexpression *e* (the default). |
| `noskip⁠[e]` | Turns off all whitespace skipping for subexpression *e*, including preceeding whitespace. |
| `lexeme⁠[e]` | Treats subexpression *e* as a lexical token with no internal whitespace skipping. |
| `repeat(N)⁠[e]` | Matches exactly *N* occurences of expression *e*. |
| `repeat(N,M)⁠[e]` | Matches at least *N* and at most *M* occurences of expression *e*. |
| `on(C)⁠[e]` | Sets the condition *C* to true for the scope of subexpression *e*. |
| `off(C)⁠[e]` | Sets the condition *C* to false for the scope of subexpression *e* (the default). |
| `symbol(S)⁠[e]` | Pushes a symbol definition for symbol *S* with value equal to the captured input matching subexpression *e*. |
| `block⁠[e]` | Creates a scope block for subexpression *e* where all new symbols defined in *e* are local to it and all external symbols defined outside of the block are also available for reference within *e*. |
| `local⁠[e]` | Creates a local scope block for subexpression *e* where all new symbols defined in *e* are local to it and there are no external symbol definitions available for reference. |
| `local(S)⁠[e]` | Creates a local scope block for subexpression *e* where all new symbols defined in *e* are local to it and all external symbols defined outside of the block are also available for reference within *e*, except for the symbol named *S*. |
| `collect<C>⁠[e]` | Synthesizes a collection attribute of container type *C* from the attributes inherited from or synthesized within expression *e*. |
| `collect<C,A...>⁠[e]` | Synthesizes a collection attribute of container type *C* consisting of elements, each of which are constructed from sequences of attributes inherited from or synthesized within expression *e* and that match the types of parameter pack *A...*. |
| `synthesize<T,A...>⁠[e]` | Synthesizes an object of type *T* constructed from a sequence of attributes inherited from or synthesized within expression *e* and that match the types of parameter pack *A...*. |
| `synthesize_shared<T>⁠[e]` | Synthesizes an object of type `std::shared_ptr<T>` by calling `std::make_shared` passing in an attribute of type *T* inherited from or synthesized within expression *e*. |
| `synthesize_shared<T,A...>⁠[e]` | Synthesizes an object of type `std::shared_ptr<T>` by calling `std::make_shared` passing in a sequence of attributes inherited from or synthesized within expression *e* and that match the types of parameter pack *A...*. |
| `synthesize_unique<T>⁠[e]` | Synthesizes an object of type `std::unique_ptr<T>` by calling `std::make_unique` passing in an attribute of type *T* inherited from or synthesized within expression *e*. |
| `synthesize_unique<T,A...>⁠[e]` | Synthesizes an object of type `std::unique_ptr<T>` by calling `std::make_unique` passing in a sequence of attributes inherited from or synthesized within expression *e* and that match the types of parameter pack *A...*. |

| Factory | Description |
| --- | --- |
| `sync(p)` | Makes a recovery rule expression that synchronizes the token string until it finds pattern *p* and returns `error_response::resume`. |
| `sync<r>(p)` | Makes a recovery rule expression that synchronizes the token string until it finds pattern *p* and returns `error_response` enumerator value *r*. |
| `sync_with_value(p,v)` | Makes a recovery rule expression that synchronizes the token string until it finds pattern *p*, emits the value *v* into the attribute stack and returns `error_response::resume`. |
| `sync_with_value<r>(p,v)` | Makes a recovery rule expression that synchronizes the token string until it finds pattern *p*, emits the value *v* into the attribute stack and returns `error_response` enumerator value *r*. |
| `with_value(v)` | Makes a recovery rule expression that emits the value *v* into the attribute stack and returns `error_response::resume`. |
| `with_value<r>(v)` | Makes a recovery rule expression that emits the value *v* into the attribute stack and returns `error_response` enumerator value *r*. |
| `with_response<r>()` | Makes a recovery rule expression that returns `error_response` enumerator value *r*. |

| Terminal | Description |
| --- | --- |
| `nop` | No operation, does not emit any instructions. |
| `eps` | Matches the empty string. |
| `eoi` | Matches the end of the input sequence. |
| `eol` | Matches a Unicode line-ending. |
| `cut` | Emits a cut operation, accepting semantic actions up to current match prefix unless there were syntax errors, and draining the input source. |
| `accept` | Accepts all semantic actions up to current match prefix, even after recovering from syntax errors. Does not drain the input source. |
| `raise⁠(f)` | Raises the labeled failure *f* to be handled by the top level error handler and recovery rule. |
| `raise⁠(f,r)` | Raises the labeled failure *f* with recovery rule *r* to be handled by the top level error handler. |
| `chr(c)` | Matches the UTF-8, UTF-16, or UTF-32 character *c*. |
| `chr(c1, c2)` | Matches characters in the UTF-8, UTF-16, or UTF-32 interval \[*c1*-*c2*\]. |
| `str(s)` | Matches the sequence of characters in the string *s*. |
| `bre(s)` | POSIX Basic Regular Expression (BRE). |
| `any` | Matches any single character. |
| `any(flags)` | Matches a character exhibiting any of the character properties. |
| `all(flags)` | Matches a character with all of the character properties. |
| `none(flags)` | Matches a character with none of the character properties. |
| `alpha` | Matches any alphabetical character. |
| `alnum` | Matches any alphabetical character or numerical digit. |
| `blank` | Matches any space or tab character. |
| `cntrl` | Matches any control character. |
| `digit` | Matches any decimal digit. |
| `graph` | Matches any graphical character. |
| `lower` | Matches any lowercase alphabetical character. |
| `print` | Matches any printable character. |
| `punct` | Matches any punctuation character. |
| `space` | Matches any whitespace character. |
| `upper` | Matches any uppercase alphabetical character. |
| `xdigit` | Matches any hexadecimal digit. |
| `when⁠(C)` | Matches if the condition named *C* is *true*, without consuming input. |
| `unless⁠(C)` | Matches if the condition named *C* is *false*, without consuming input. |
| `exists⁠(S)` | Matches if there is a definition for symbol *S* in the current scope. |
| `missing⁠(S)` | Matches if there is no definition for symbol *S* in the current scope. |
| `match⁠(S)` | Matches the last definition for symbol named *S*. |
| `match_any⁠(S)` | Matches against any prior definition for symbol named *S*. |
| `match_all⁠(S)` | Matches against all prior definitions for symbol named *S*, in sequence from least to most recent. |
| `match_front⁠(S,N=0)` | Matches against the *N*-th least recent definition for symbol named *S*. |
| `match_back⁠(S,N=0)` | Matches against the *N*-th most recent definition for symbol named *S*. |

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