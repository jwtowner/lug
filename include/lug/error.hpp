// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#ifndef LUG_INCLUDE_LUG_ERROR_HPP
#define LUG_INCLUDE_LUG_ERROR_HPP

#include <lug/config.hpp>

#include <cerrno>
#include <cstdlib>
#include <stdexcept>
#include <string>

// NOLINTBEGIN(cppcoreguidelines-macro-usage)

#ifndef LUG_NO_EXCEPTIONS
#define LUG_TRY try
#define LUG_CATCH(...) catch (__VA_ARGS__)
#define LUG_CATCH_ANY catch (...)
#define LUG_RETHROW throw
#else
#define LUG_TRY
#define LUG_CATCH(...) if ([[maybe_unused]] __VA_ARGS__ = {}; (lug::exceptions_enabled))
#define LUG_CATCH_ANY if ((lug::exceptions_enabled))
#define LUG_RETHROW
#endif

// NOLINTEND(cppcoreguidelines-macro-usage)

namespace lug {

#ifdef _PREFAST_
inline bool const volatile
#else 
constexpr bool
#endif
exceptions_enabled =
#ifndef LUG_NO_EXCEPTIONS
true
#else
false
#endif
;

template <class Error, class... Args>
[[noreturn]] void throw_exception(Args&&... args)
{
#ifndef LUG_NO_EXCEPTIONS
	throw Error{std::forward<Args>(args)...};
#else
	errno = ENOTRECOVERABLE;
	std::perror(Error{std::forward<Args>(args)...}.what());
	std::abort();
#endif
}

class lug_error : public std::runtime_error { using std::runtime_error::runtime_error; };
class invalid_argument : public lug_error { using lug_error::lug_error; };
class program_limit_error : public lug_error { public: program_limit_error() : lug_error{"length or offset of program exceeds internal limit"} {} };
class resource_limit_error : public lug_error { public: resource_limit_error() : lug_error{"number of resources exceeds internal limit"} {} };
class reenterant_parse_error : public lug_error { public: reenterant_parse_error() : lug_error{"parsing is non-reenterant"} {} };
class reenterant_read_error : public lug_error { public: reenterant_read_error() : lug_error{"attempted to read or modify input source while reading"} {} };
class attribute_stack_error : public lug_error{ public: attribute_stack_error() : lug_error{"incompatible or invalid attribute stack frame"} {} };
class bad_string_expression : public lug_error { public: explicit bad_string_expression(std::string const& s = "invalid string or bracket expression") : lug_error{s} {} };
class bad_character_class : public bad_string_expression { public: bad_character_class() : bad_string_expression{"invalid character class"} {} };
class bad_character_range : public bad_string_expression { public: bad_character_range() : bad_string_expression{"character range is reversed"} {} };
class bad_grammar : public lug_error { public: bad_grammar() : lug_error{"invalid or empty grammar"} {} };
class bad_move_only_any_cast : public lug_error { public: bad_move_only_any_cast() : lug_error{"bad move_only_any cast"} {} };
class bad_opcode : public lug_error { public: bad_opcode() : lug_error{"invalid opcode"} {} };
class bad_stack : public lug_error{ public: bad_stack() : lug_error{"empty or invalid parser stack error"} {} };

} // namespace lug

#endif
