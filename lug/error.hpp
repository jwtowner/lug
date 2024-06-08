// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017 Jesse W. Towner
// See LICENSE.md file for license details

#ifndef LUG_INCLUDE_LUG_ERROR_HPP
#define LUG_INCLUDE_LUG_ERROR_HPP

#include <stdexcept>

namespace lug
{

class lug_error : public std::runtime_error { using std::runtime_error::runtime_error; };
class program_limit_error : public lug_error { public: program_limit_error() : lug_error{"length or offset of program exceeds internal limit"} {} };
class resource_limit_error : public lug_error { public: resource_limit_error() : lug_error{"number of resources exceeds internal limit"} {} };
class reenterant_parse_error : public lug_error { public: reenterant_parse_error() : lug_error{"parsing is non-reenterant"} {} };
class reenterant_read_error : public lug_error { public: reenterant_read_error() : lug_error{"attempted to read or modify input source while reading"} {} };
class reenterant_accept_error : public lug_error { public: reenterant_accept_error() : lug_error{"accepting parse input is non-reenterant" } {} };
class accept_context_error : public lug_error{ public: accept_context_error() : lug_error{"operation valid only inside calling context of parser::accept"} {} };
class bad_string_expression : public lug_error { public: bad_string_expression(const char* s = "invalid string or bracket expression") : lug_error{s} {} };
class bad_character_class : public bad_string_expression { public: bad_character_class() : bad_string_expression{"invalid character class"} {} };
class bad_character_range : public bad_string_expression { public: bad_character_range() : bad_string_expression{"character range is reversed"} {} };
class bad_grammar : public lug_error { public: bad_grammar() : lug_error{"invalid or empty grammar"} {} };
class bad_opcode : public lug_error { public: bad_opcode() : lug_error{"invalid opcode"} {} };

} // namespace lug

#endif
