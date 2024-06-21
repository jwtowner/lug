// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2024 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>

// Matcher for JSON Data Interchange Standard (RFC7159)
class json_matcher
{
public:
	json_matcher()
	{
		using namespace lug::language;
		rule JSON;
		rule ExponentPart   = lexeme[ "[Ee]"_rx > ~"[+-]"_rx > +"[0-9]"_rx ];
		rule FractionalPart = lexeme[ "."_sx > +"[0-9]"_rx ];
		rule IntegralPart   = lexeme[ "0"_sx | "[1-9]"_rx > *"[0-9]"_rx ];
		rule Number         = lexeme[ ~"-"_sx > IntegralPart > ~FractionalPart > ~ExponentPart ];
		rule Boolean        = lexeme[ "true"_sx | "false" ];
		rule Null           = lexeme[ "null" ];
		rule UnicodeEscape  = lexeme[ chr('u') > "[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]"_rx ];
		rule Escape         = lexeme[ "\\" > ("[/\\bfnrt]"_rx | UnicodeEscape) ];
		rule String         = lexeme[ "\"" > *("[^\"\\\u0000-\u001F]"_rx | Escape) > "\"" ];
		rule Array          = "[" > JSON > *("," > JSON) > "]";
		rule Object         = "{" > String > ":" > JSON > *("," > String > ":" > JSON) > "}";
		JSON                = Object | Array | String | Number | Boolean | Null;
		grammar_ = start(JSON);
	}

	bool parse(std::istream& input)
	{
		return lug::parse(input, grammar_);
	}

private:
	lug::grammar grammar_;
};

int main()
{
	try {
		json_matcher matcher;
		if (!matcher.parse(std::cin)) {
			std::cout << "Invalid JSON!\n";
			return -1;
		}
	} catch (std::exception const& e) {
		std::cerr << "ERROR: " << e.what() << "\n";
		return -1;
	} catch (...) {
		std::cerr << "UNKNOWN ERROR\n";
		return -1;
	}
	return 0;
}
