// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug.hpp>

class json_parser
{
public:
	json_parser()
	{
		using namespace lug::language;
		rule JSON;
		rule Space = *(u8"[\u0009\u000A\u000D\u0020]"s);
		rule ExponentPart = "[eE]" > ~"[+-]"s > +"[0-9]"s;
		rule FractionalPart = "[.]" > +"[0-9]"s;
		rule IntegralPart = "0" | "[1-9]" > *"[0-9]"s;
		rule Number = ~"-"s > IntegralPart > ~FractionalPart > ~ExponentPart;
		rule Boolean = "true"s | "false";
		rule Null = "null";
		rule UnicodeEscape = "u" > +"[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]"s;
		rule Escape = "\\" > ("[/\\bfnrt]" | UnicodeEscape);
		rule String = Space > "\"" > *(u8"[^\"\\\u0000-\u001F]"s | Escape) > "\"" > Space;
		rule Array = "[[]" > ((JSON > *("," > JSON)) | Space) > "[]]";
		rule Object = "{" > (String > ":" > JSON > *("," > String > ":" > JSON) | Space) > "}";
		JSON = Space > (Object | Array | String | Number | Boolean | Null) > Space;
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
	}
	catch (std::exception& e) {
		std::cerr << "Error: " << e.what() << std::endl;
		return -1;
	}
	return 0;
}
