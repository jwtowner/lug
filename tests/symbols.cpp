// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2024 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>

#undef NDEBUG
#include <cassert>

void test_symbol_match()
{
	using namespace lug::language;
	rule Name = lexeme[alpha > *alnum];
	rule Xml = chr('<') > Name > chr('>') > ~Xml > str("</") > match("TAG") > chr('>');
	grammar G = start(Xml > eoi);

	environment E;
	E.add_symbol("TAG", "a");

	assert(lug::parse("<a></a>", G, E));
	assert(lug::parse("< a ></ a>", G, E));
	assert(!lug::parse("<a></b>", G, E));

	E.clear_symbols("TAG");
	E.add_symbol("TAG", "b");

	assert(lug::parse("<b></b>", G, E));
	assert(lug::parse("<b ></ b>", G, E));
	assert(!lug::parse("<b ></a>", G, E));
	assert(!lug::parse("<b ></ a>", G, E));

	E.add_symbol("TAG", "body");

	assert(lug::parse("<body></body>", G, E));
	assert(lug::parse("<body></ body>", G, E));
	assert(lug::parse("< body></body >", G, E));
	assert(!lug::parse("<body></broken>", G, E));
	assert(!lug::parse("<body></ broken>", G, E));
}

void test_symbol_definition_and_match()
{
	using namespace lug::language;
	rule Name = lexeme[alpha > *alnum];
	rule Xml = chr('<') > symbol("TAG")[Name] > chr('>') > ~Xml > str("</") > match("TAG") > chr('>');
	grammar G = start(Xml > eoi);

	assert(lug::parse("<a></a>", G));
	assert(lug::parse("<a></ a>", G));
	assert(lug::parse("< a></a>", G));
	assert(lug::parse("< b></b>", G));
	assert(lug::parse("<b></ b>", G));
	assert(lug::parse("<b ></ b >", G));
	assert(lug::parse("< b ></b>", G));
	assert(lug::parse("<body></body>", G));
	assert(lug::parse("<body></ body>", G));
	assert(lug::parse("< body></body >", G));

	assert(!lug::parse("<a></b>", G));
	assert(!lug::parse("<a></aa>", G));
	assert(!lug::parse("<aa></a>", G));
	assert(!lug::parse("<b></a>", G));
	assert(!lug::parse("< b></ a>", G));
	assert(!lug::parse("<b ></a>", G));
	assert(!lug::parse("<body></broken>", G));
	assert(!lug::parse("<body></ broken>", G));
}

void test_symbol_definition_and_match_2()
{
	using namespace lug::language;
	rule Name = lexeme[+alpha];
	rule FruitSet = symbol("fruit")[Name] > match("fruit");
	grammar G = start(FruitSet > eoi);

	assert(lug::parse("Apple Apple", G));
	assert(lug::parse("Orange Orange", G));
	assert(lug::parse("Banana Banana", G));

	assert(!lug::parse("Apple", G));
	assert(!lug::parse("Orange", G));
	assert(!lug::parse("Banana", G));
	assert(!lug::parse("Apple Orange", G));
	assert(!lug::parse("Banana Orange", G));
	assert(!lug::parse("Banana Apple", G));
}

void test_symbol_definition_and_match_3()
{
	using namespace lug::language;
	rule Name = lexeme[+alpha];
	rule FruitSet = symbol("fruit")[Name] > symbol("fruit")[Name] > match("fruit");
	grammar G = start(FruitSet > eoi);

	assert(lug::parse("Apple Apple Apple", G));
	assert(lug::parse("Orange Orange Orange", G));
	assert(lug::parse("Banana Banana Banana", G));
	assert(lug::parse("Apple Orange Orange", G));
	assert(lug::parse("Banana Orange Orange", G));
	assert(lug::parse("Banana Apple Apple", G));

	assert(!lug::parse("Apple Apple", G));
	assert(!lug::parse("Orange Orange", G));
	assert(!lug::parse("Banana Banana", G));
	assert(!lug::parse("Apple Orange Apple", G));
	assert(!lug::parse("Banana Orange Banana", G));
	assert(!lug::parse("Banana Apple Banana", G));
	assert(!lug::parse("Banana Orange Apple", G));
	assert(!lug::parse("Apple Orange Banana", G));
	assert(!lug::parse("Orange Apple Banana", G));
	assert(!lug::parse("Orange Apple Kiwi", G));
	assert(!lug::parse("Orange Apple Watermelon", G));
}

void test_symbol_definition_and_match_any()
{
	using namespace lug::language;
	rule Name = lexeme[+alpha];
	rule FruitSet = symbol("fruit")[Name] > symbol("fruit")[Name] > match_any("fruit");
	grammar G = start(FruitSet > eoi);

	assert(lug::parse("Apple Apple Apple", G));
	assert(lug::parse("Orange Orange Orange", G));
	assert(lug::parse("Banana Banana Banana", G));
	assert(lug::parse("Apple Orange Orange", G));
	assert(lug::parse("Banana Orange Orange", G));
	assert(lug::parse("Banana Apple Apple", G));
	assert(lug::parse("Apple Orange Apple", G));
	assert(lug::parse("Banana Orange Banana", G));
	assert(lug::parse("Banana Apple Banana", G));
	assert(lug::parse("Apple Banana Banana", G));
	assert(lug::parse("Orange Banana Orange", G));
	assert(lug::parse("Orange Apple Apple", G));
	assert(lug::parse("Orange Apple Orange", G));

	assert(!lug::parse("Apple Apple", G));
	assert(!lug::parse("Orange Orange", G));
	assert(!lug::parse("Banana Banana", G));
	assert(!lug::parse("Apple Orange Banana", G));
	assert(!lug::parse("Banana Orange Apple", G));
	assert(!lug::parse("Orange Apple Banana", G));
	assert(!lug::parse("Apple Orange Kiwi", G));
	assert(!lug::parse("Banana Orange Watermelon", G));
}

void test_symbol_definition_and_match_all()
{
	using namespace lug::language;
	rule Name = lexeme[+alpha];
	rule FruitSet = symbol("fruit")[Name] > symbol("fruit")[Name] > match_all("fruit");
	grammar G = start(FruitSet > eoi);

	assert(lug::parse("Apple Apple AppleApple", G));
	assert(lug::parse("Orange Orange OrangeOrange", G));
	assert(lug::parse("Banana Banana BananaBanana", G));
	assert(lug::parse("Apple Orange AppleOrange", G));
	assert(lug::parse("Banana Orange BananaOrange", G));
	assert(lug::parse("Banana Apple BananaApple", G));
	assert(lug::parse("Apple Orange AppleOrange", G));
	assert(lug::parse("Banana Orange BananaOrange", G));
	assert(lug::parse("Banana Apple BananaApple", G));
	assert(lug::parse("Apple Banana AppleBanana", G));
	assert(lug::parse("Orange Banana OrangeBanana", G));
	assert(lug::parse("Orange Apple OrangeApple", G));

	assert(!lug::parse("Apple Apple", G));
	assert(!lug::parse("Orange Orange", G));
	assert(!lug::parse("Banana Banana", G));
	assert(!lug::parse("Apple Orange Banana", G));
	assert(!lug::parse("Apple Orange OrangeApple", G));
	assert(!lug::parse("Banana Orange Apple", G));
	assert(!lug::parse("Orange Apple Banana", G));
	assert(!lug::parse("Apple Orange Kiwi", G));
	assert(!lug::parse("Banana Orange Watermelon", G));
	assert(!lug::parse("Orange Apple AppleOrange", G));
}

void test_symbol_nested_definition_and_match()
{
	using namespace lug::language;
	rule Inner;
	rule Name = lexeme[alpha > *alnum];
	rule Xml = chr('<') > symbol("TAG")[Name] > chr('>') > ~Inner > str("</") > match("TAG") > chr('>');
	Inner = block[Xml];
	grammar G = start(Xml > eoi);

	assert(lug::parse("<a></a>", G));
	assert(lug::parse("<a></ a>", G));
	assert(lug::parse("< a></a>", G));
	assert(lug::parse("< b><a></a></b>", G));
	assert(lug::parse("<b><c ></c></ b>", G));
	assert(lug::parse("<b >< d></d ></ b >", G));
	assert(lug::parse("< b ><food></food ></b>", G));
	assert(lug::parse("<body></body>", G));
	assert(lug::parse("<body></ body>", G));
	assert(lug::parse("< body><ul></ul></body >", G));
	assert(lug::parse("<body><ul><li></li></ul></body>", G));

	assert(!lug::parse("<a></b>", G));
	assert(!lug::parse("<a></aa>", G));
	assert(!lug::parse("<aa></a>", G));
	assert(!lug::parse("<b></a>", G));
	assert(!lug::parse("<a><b></a>", G));
	assert(!lug::parse("< b><c></ a>", G));
	assert(!lug::parse("<b ><c></d></a>", G));
	assert(!lug::parse("<body><ul><li></lx></ul></body>", G));
	assert(!lug::parse("<body><ul><li></ul></li></body>", G));
	assert(!lug::parse("<body></broken>", G));
	assert(!lug::parse("<body></ broken>", G));
}

int main()
{
	try {
		test_symbol_match();
		test_symbol_definition_and_match();
		test_symbol_definition_and_match_2();
		test_symbol_definition_and_match_3();
		test_symbol_definition_and_match_any();
		test_symbol_definition_and_match_all();
		test_symbol_nested_definition_and_match();
	} catch (std::exception& e) {
		std::cerr << "Error: " << e.what() << "\n";
		return -1;
	}
	return 0;
}
