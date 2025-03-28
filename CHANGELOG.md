# Changelog

## Release v0.5.0 (Under Development)

* Implemented collection and object attribute directives. The new `collect<C>[e]` directive synthesizes a sequence or associative container type `C` consisting of elements gathered from the inherited or synthesized attributes in expression `e`. Likewise, there also new `synthesize<C,A...>[e]`, `synthesize_shared<C,A...>[e]` and `synthesize_unique<C,A...>[e]` directives for synthesizing objects, shared pointers and unique pointers respectively, constructed from the component attributes in expression `e`.
* Implemented `synthesize_collect` directive that combines `collect` and `synthesize` directives together for improved code readability and reduced boilerplate when constructing complex data structures from parsed elements. This is particularly useful for building nested collections like arrays of objects or maps with complex value types.
* Added `lug::recursive_wrapper` template class for handling circular dependencies in abstract syntax trees, particularily those making use of `std::variant`.
* Updated to Unicode Standard Version 16.0.0 and added support for building tools to CMakeLists.txt.
* Optimized character range and rune set matching for ASCII characters, resulting in significant performance improvements for common text processing operations. Added specialized fast paths for ASCII-only inputs that bypass the more expensive Unicode handling code.
* Implemented test opcodes for head-fail optimization and repeat opcodes for optimized whitespace skipping. These optimizations will be fully enabled in the next release after planned expression tree transformations.
* Enhanced input source handling with improved buffering and error reporting for `std::istream` sources, and improved the interactive mode support that properly handles line-by-line input for terminal sessions or for line-oriented grammars.
* Moved `std::istream` support into its own `<lug/iostream.hpp>` header file, removing the dependency on `<iostream>` from the main `<lug/lug.hpp>` header. This reduces compilation times and minimizes header dependencies for projects that don't require stream I/O functionality.
* Refactored parser commit logic by inlining instructions in `lug::basic_parser` to better align with the stack frame architecture changes introduced in v0.4.0, improving code organization and performance.
* Fixed issue in the BASIC sample where user-defined functions (e.g. FNA(X)) were crashing during evaluation. This was caused by v0.4.0 changes that reset `lug::environment` during nested parsing operations. Added new `lug::environment::should_reset_on_parse` member function to provide fine-grained control over this behavior, allowing environments to persist across nested parse operations when needed.
* Added comprehensive test infrastructure for the sample programs, including automated test plans and verification scripts to ensure sample functionality remains correct across changes.
* Restructured include directory hierarchy by moving `lug/` to `include/lug/`, ensuring cleaner include paths and better organization of header files.
* Added support for additional compilers (GCC 9/10/11/12, Clang 14/15/16/17) in the C/C++ CI workflow to ensure broader compatibility.
* Added static analyzer CI workflow for Clang and MSVC to detect potential bugs, memory leaks, and undefined behavior during the build process.
* Added compiler sanitizer CI workflow with Address Sanitizer (ASan), Undefined Behavior Sanitizer (UBSan), and Memory Sanitizer (MSan) to detect memory errors, undefined behavior, and other runtime issues early in the development process.
* Added clang-tidy integration to CI workflow to enforce code quality standards and catch potential issues early in the development process.
* Removed Ubuntu 20.04 from the GitHub workflows as it has reached end-of-life and is no longer supported.

## Release v0.4.0 (March 4, 2025)

* Added new error handling and recovery with labeled failures, recovery rules and error handlers.
* Added new `accept` operator that works in conjuction with `cut` operator. Tested and fixed issues with `accept` and `cut`, allowing them to be used for debugging error handling.
* Added `repeat(N)[e]` and `repeat(N,M)[e]` control operators that repeat matching of expression `e` at least `N` and at most `M` times.
* Added `on_reset` and `on_drain` hooks to the `lug::environment` class.
* Refactored the encoder expression interfaces into CRTP base classes to allow for common functionality to be shared by all concrete encoder expressions, such as support for labeled failures and recovery rules.
* Implemented new 64-bit instruction encoding scheme that is simpler and more efficient. String data is no longer embedded in the instruction stream, ensuring constant instruction length of 8 bytes.
* Merged `lug::program_encoder` and `lug::rule_encoder` into the `lug::encoder` base class after overhauling how choice/jump offsets are calculated, significantly reducing binary size bloat.
* Extracted common base class of `lug::basic_parser` into `lug::parser_base` to reduce template bloat.
* Reduced template bloat for parser directives (improving compiler error messages) and optimized nested directives.
* Merged parser stack frames together into a single stack using `std::variant`.
* Fixed issues with `lug::basic_parser` and `lug::environment` not being fully reset on each invocation of `parse()`.
* Allow for move-only types to be used in the `lug::environment` attribute stack.
* Removed use of `goto` in parsing machine main loop.
* Changed grammar program generation to force inline the start rule.
* Implemented tail call optimization.

## Release v0.3.0 (July 4, 2024)

* Added list repetition operator `e1 >> e2` to the DSL that is shorthand for `e1 > *(e2 > e1)`.
* Added support for parsing characters and character literals where applicable without explicitly needing to wrap them with `chr()` or `_cx`.
* Symbols now respects `caseless` mode, allowing for case-insensitive matching against symbol definitions.
* Allow for use of variables of all types in attribute bindings and removed the `lug::variable` template class that was used previously. Variable state is automatically saved and restored across rule boundaries.
* Allow for capturing text to a `lug::syntax` object or any string-like object that is convertible from `std::string_view`, and renamed `syntax::capture` to `syntax::str` in order to match `std::sub_match::str`.
* Added `lug::source_options::interactive` flag that ignores `eoi` tokens for TTY input sources.
* Rewrote the expression function objects/lambdas as expression template classes. Allows for multiple passes over the expression tree as well as top-down and bottom-up traversal, which was needed when implementing attribute state tracking. This will also allow for additional optimizations to be implemented in the future.
* Renamed `syntactic_capture` to `semantic_capture_action` to reflect that it is executed during the semantic action evaluation phase.
* Make all variations of callables that return a non-void value that can be type-erased by `semantic_action` and `semantic_capture_action` push their result onto the attribute result stack.
* Removed `semantic_response` from the public API as it was only used internally inside of the parser.
* Attempting to bind a variable to a nonexistent value from the attribute result stack now throws an `attribute_stack_error`.
* `implicit_space_rule` no longer causes a compiler warning with Clang, uses RAII to push/pop the thread-local white space rule for grammars.
* Moved `call_depth()`, `prune_depth()` and `escape()` functions into the `lug::environment` class since they are used exclusively during semantic action phase.
* Moved line/column tracking and current match/subject string views to `lug::environment` class, fully removing the environment's dependency on `lug::parser`.
* Turned `lug::parser` into an alias of a new `lug::basic_parser` template class parameterized with an input source strategy. This allows for parsing and capturing of text without making a copy of the input.
* Placed all DSL operator overloads inside of an inline namespace `operators` within `lug::language`. This allows only the operators to be imported into the current scope if desired.
* Enabled `-Wconversion` and `-Wshadow` warnings for Clang and GCC and fixed warnings.
* Full clang-tidy pass on all of the library headers and fixed all warnings.
* Added CMake build support and removed old MSVS solution and vcxproj files.
* Handle situation where compilation with RTTI is disabled.

## Release v0.2.0 (June 21, 2024)

* Implemented new support for context-sensitive grammars with symbol tables and parsing conditions based on the PEG extensions described in the paper *"A Declarative Extension of Parsing Expression Grammars for Recognizing Most Programming Languages"* by Tetsuro Matsumura and Kimio Kuramitsu (2015).
* Added an XML Standard 1.0 matcher sample program demonstrating use of symbol tables.
* Finished the BASIC language interpreter sample program, which is now feature complete, using parsing conditions.
* Updated Unicode support to version 15.1.0 and automated Unicode table generation via Makefile build.
* Various compilation error fixes for modern C++17 compilers (Clang, GCC, MSVC++), as well as fixing compilation errors when compiling in C++20 and C++23 modes. Includes fixes merged from @trombonehero.
* Code cleanup pass, with numerous improvements applying `const`, `constexpr`, `[[nodiscard]]`, `noexcept` and fixing new compilation warnings enabled with `-Wextra`, `-Wextra-semi`, `-Wsign-conversion` and `-Wsuggest-override` on Clang and GCC.
* Initial clang-tidy support with .clang-tidy file and lint Makefile target.
* Migrated from Travis CI to Github Actions CI.

## Release v0.1.0 (December 10, 2017)

* Initial Release.