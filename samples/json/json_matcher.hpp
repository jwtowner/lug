// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#ifndef LUG_SAMPLES_JSON_JSON_MATCHER_HPP
#define LUG_SAMPLES_JSON_JSON_MATCHER_HPP

#include <lug/lug.hpp>
#include <lug/iostream.hpp>

// Matcher for JSON Data Interchange Standard (RFC7159)
class json_matcher
{
public:
	json_matcher()
	{
		using namespace lug::language;
		rule JSON;
		auto ExponentPart   = lexeme[ "Ee"_bx > ~"+-"_bx > +"0-9"_bx ];
		auto FractionalPart = lexeme[ '.'_cx > +"0-9"_bx ];
		auto IntegralPart   = lexeme[ '0'_cx | "1-9"_bx > *"0-9"_bx ];
		auto Number         = lexeme[ ~'-'_cx > IntegralPart > ~FractionalPart > ~ExponentPart ];
		auto Boolean        = lexeme[ "true"_sx | "false" ];
		auto Null           = lexeme[ "null" ];
		auto UnicodeEscape  = lexeme[ 'u' > exactly<4>[ "0-9A-Fa-f"_bx ] ];
		auto Escape         = lexeme[ '\\' > ("/\"\\bfnrt"_bx | UnicodeEscape) ];
		rule String         = lexeme[ '"' > *("^\"\\\u0000-\u001F"_bx | Escape) > '"' ];
		auto Array          = '[' > JSON >> ',' > ']';
		auto Object         = '{' > (String > ':' > JSON) >> ',' > '}';
		JSON                = Object | Array | String | Number | Boolean | Null;
		grammar_            = start(JSON > eoi);
	}

	template <typename T>
	bool match(T&& t) const
	{
		return lug::parse(std::forward<T>(t), grammar_);
	}

	bool match_cin() const
	{
		return lug::parse(grammar_);
	}

private:
	lug::grammar grammar_;
};

#endif // LUG_SAMPLES_JSON_JSON_MATCHER_HPP
