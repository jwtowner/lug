// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>

class json_parser
{
public:
	json_parser()
	{
		using namespace lug::language;
		rule JSON;
		rule ExponentPart   = lexeme[ "[Ee]" > ~"[+-]"s > +"[0-9]"s ];
		rule FractionalPart = lexeme[ "[.]" > +"[0-9]"s ];
		rule IntegralPart   = lexeme[ "0" | "[1-9]" > *"[0-9]"s ];
		rule Number         = lexeme[ ~"-"s > IntegralPart > ~FractionalPart > ~ExponentPart ];
		rule Boolean        = lexeme[ "true"s | "false"s ];
		rule Null           = lexeme[ "null" ];
		rule UnicodeEscape  = lexeme[ chr('u') > "[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]"s ];
		rule Escape         = lexeme[ "\\" > ("[/\\bfnrt]" | UnicodeEscape) ];
		rule String         = lexeme[ "\"" > *(u8"[^\"\\\u0000-\u001F]"s | Escape) > "\"" ];
		rule Array          = "[[]" > JSON > *("," > JSON) > "[]]";
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
		json_parser parser;
		if (!parser.parse(std::cin)) {
			std::cout << "Invalid JSON!" << std::endl;
			return -1;
		}
	} catch (std::exception& e) {
		std::cerr << "Error: " << e.what() << std::endl;
		return -1;
	}
	return 0;
}
