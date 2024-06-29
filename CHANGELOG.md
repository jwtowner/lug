# Changelog

## Release v0.3.0 (Under Development)

* Removed the `lug::variable` template class and instead allow for natural use of types and variables in captures and attribute bindings. Variable state is automatically saved and restored across rule boundaries.
* Rewrote the expression objects as expression template classes rather than lambdas while implementing attribute state tracking. Will allow for additional optimizations into the future.
* Allow for capturing text to a `lug::syntax` object or any string-like object that is convertible from `std::string_view`.
* `implicit_space_rule` no longer causes a compiler warning with Clang, uses RAII to push/pop the thread-local white space rule for grammars.
* Moved `call_depth()`, `prune_depth()` and `escape()` functions into the `lug::environment` class since they are used exclusively during semantic action phase.
* Moved line/column tracking and current match/subject string views to `lug::environment` class, fully removing the environment's dependency on `lug::parser`.
* Added list repetition operator `e1 >> e2` to the DSL that is shorthand for `e1 > *(e2 > e1)`.
* Enabled `-Wconversion` and `-Wshadow` warnings for Clang and GCC and fixed warnings.
* Handle situation where compilation with RTTI is disabled.

## Release v0.2.0 (June 21, 2024)

* Feature: Implemented new support for context-sensitive grammars with symbol tables and parsing conditions, based on the PEG extensions described in the paper *"A Declarative Extension of Parsing Expression Grammars for Recognizing Most Programming Languages"* by Tetsuro Matsumura and Kimio Kuramitsu (2015).
* Added an XML Standard 1.0 matcher sample program demonstrating use of symbol tables.
* Finished the BASIC language interpreter sample program, which is now feature complete, using parsing conditions.
* Updated Unicode support to version 15.1.0 and automated Unicode table generation via Makefile build.
* Various compilation error fixes for modern C++17 compilers (Clang, GCC, MSVC++), as well as fixing compilation errors when compiling in C++20 and C++23 modes. Includes fixes merged from @trombonehero.
* Code cleanup pass, with numerous improvements applying `const`, `constexpr`, `[[nodiscard]]`, `noexcept` and fixing new compilation warnings enabled with `-Wextra`, `-Wextra-semi`, `-Wsign-conversion` and `-Wsuggest-override` on Clang and GCC.
* Initial clang-tidy support with .clang-tidy file and lint Makefile target.
* Migrated from Travis CI to Github Actions CI.

## Release v0.1.0 (December 10, 2017)

* Initial Release.