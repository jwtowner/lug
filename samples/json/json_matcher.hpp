// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#ifndef LUG_SAMPLES_JSON_JSON_MATCHER_HPP
#define LUG_SAMPLES_JSON_JSON_MATCHER_HPP

#include <lug/lug.hpp>

// Matcher for JSON Data Interchange Standard (RFC7159)
class json_matcher
{
public:
	json_matcher()
	{
		using namespace lug::language;
		rule JSON;
		auto ExponentPart   = lexeme[ "[Ee]"_rx > ~"[+-]"_rx > +"[0-9]"_rx ];
		auto FractionalPart = lexeme[ "."_sx > +"[0-9]"_rx ];
		auto IntegralPart   = lexeme[ "0"_sx | "[1-9]"_rx > *"[0-9]"_rx ];
		auto Number         = lexeme[ ~"-"_sx > IntegralPart > ~FractionalPart > ~ExponentPart ];
		auto Boolean        = lexeme[ "true"_sx | "false" ];
		auto Null           = lexeme[ "null" ];
		auto UnicodeEscape  = lexeme[ 'u' > "[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]"_rx ];
		auto Escape         = lexeme[ '\\' > ("[/\"\\bfnrt]"_rx | UnicodeEscape) ];
		rule String         = lexeme[ '"' > *("[^\"\\\u0000-\u001F]"_rx | Escape) > '"' ];
		auto Array          = '[' > JSON > *(',' > JSON) > ']';
		auto Object         = '{' > String > ':' > JSON > *(',' > String > ':' > JSON) > '}';
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
