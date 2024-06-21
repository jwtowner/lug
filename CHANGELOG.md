# Changelog

## Release v0.2.0 (June 21, 2024)

* Feature: Implemented new support for context-sensitive grammars with symbol tables and parsing conditions, based on the PEG extensions described in the paper *"A Declarative Extension of Parsing Expression Grammars for Recognizing Most Programming Languages (2015)"* by Tetsuro Matsumura and Kimio Kuramitsu.
* Added an XML Standard 1.0 matcher sample program demonstrating use of symbol tables.
* Finished the BASIC language interpreter sample program, which is now feature complete, using parsing conditions.
* Updated Unicode support to version 15.1.0 and automated Unicode table generation via Makefile build.
* Various compilation error fixes for modern C++17 compilers (Clang, GCC, MSVC++), as well as fixing compilation errors when compiling in C++20 and C++23 modes. Includes fixes merged from @trombonehero.
* Code cleanup pass, with numerous improvements applying `const`, `constexpr`, `[[nodiscard]]`, `noexcept` and fixing new compilation warnings enabled with `-Wextra`, `-Wextra-semi`, `-Wsign-conversion` and `-Wsuggest-override` on Clang and GCC.
* Initial clang-tidy support with .clang-tidy file and lint Makefile target.
* Migrated from Travis CI to Github Actions CI.

## Release v0.1.0 (December 10, 2017)

* Initial Release.
