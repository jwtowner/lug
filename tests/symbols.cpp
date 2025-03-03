// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>

#undef NDEBUG
#include <cassert>

void test_symbol_exists()
{
	using namespace lug::language;

	int Count = 0;

	rule Name = lexeme[+alpha];
	rule OptionalName = ~symbol("Name")[Name] >
			( exists("Name") <[&Count] { ++Count; }
			| missing("Name") <[&Count] { --Count; } );
	grammar G = start(OptionalName);

	assert(Count == 0);
	assert(lug::parse("Apple", G));
	assert(Count == 1);

	Count = 0;
	assert(lug::parse("123132", G));
	assert(Count == -1);

	Count = 0;
	assert(lug::parse("Banana Banana", G));
	assert(Count == 1);
}

void test_symbol_match()
{
	using namespace lug::language;

	rule Xml, Name;
	Name = lexeme[alpha > *alnum];
	Xml = chr('<') > Name > chr('>') > ~Xml > str("</") > match("TAG") > chr('>');

	grammar G = start(Xml > eoi);

	environment E;
	assert(!E.has_symbol("TAG"));
	E.add_symbol("TAG", "a");
	assert(E.has_symbol("TAG"));

	assert(lug::parse("<a></a>", G, E));
	assert(lug::parse("< a ></ a>", G, E));
	assert(!lug::parse("<a></b>", G, E));
	assert(!lug::parse("<a></B>", G, E));
	assert(!lug::parse("<a></A>", G, E));

	assert(E.has_symbol("TAG"));
	E.clear_symbols("TAG");
	assert(!E.has_symbol("TAG"));
	E.add_symbol("TAG", "b");
	assert(E.has_symbol("TAG"));

	assert(lug::parse("<b></b>", G, E));
	assert(lug::parse("<b ></ b>", G, E));
	assert(!lug::parse("<b ></a>", G, E));
	assert(!lug::parse("<b ></ a>", G, E));
	assert(!lug::parse("<b></B>", G, E));

	assert(E.has_symbol("TAG"));
	E.add_symbol("TAG", "body");
	assert(E.has_symbol("TAG"));

	assert(lug::parse("<body></body>", G, E));
	assert(lug::parse("<body></ body>", G, E));
	assert(lug::parse("< body></body >", G, E));
	assert(!lug::parse("<body></broken>", G, E));
	assert(!lug::parse("<body></ broken>", G, E));
	assert(!lug::parse("<body></BODY>", G, E));
	assert(!lug::parse("<BODY></boDY>", G, E));
}

void test_symbol_definition_and_match()
{
	using namespace lug::language;

	rule Xml, Name;
	Name = lexeme[alpha > *alnum];
	Xml = chr('<') > symbol("TAG")[Name] > chr('>') > ~Xml > str("</") > match("TAG") > chr('>');

	grammar G = start(Xml > eoi);

	assert(lug::parse("<a></a>", G));
	assert(lug::parse("<A></A>", G));
	assert(lug::parse("<a></ a>", G));
	assert(lug::parse("< a></a>", G));
	assert(lug::parse("< b></b>", G));
	assert(lug::parse("<b></ b>", G));
	assert(lug::parse("<b ></ b >", G));
	assert(lug::parse("< b ></b>", G));
	assert(lug::parse("<body></body>", G));
	assert(lug::parse("<body></ body>", G));
	assert(lug::parse("< body></body >", G));
	assert(lug::parse("<BODY></BODY>", G));
	assert(lug::parse("<BODY></ BODY>", G));
	assert(lug::parse("< BODY></BODY >", G));

	assert(!lug::parse("<a></b>", G));
	assert(!lug::parse("<a></aa>", G));
	assert(!lug::parse("<aa></a>", G));
	assert(!lug::parse("<a></A>", G));
	assert(!lug::parse("<A></a>", G));
	assert(!lug::parse("<b></a>", G));
	assert(!lug::parse("< b></ a>", G));
	assert(!lug::parse("<b ></a>", G));
	assert(!lug::parse("<body></broken>", G));
	assert(!lug::parse("<body></ broken>", G));
	assert(!lug::parse("<body></body2>", G));
	assert(!lug::parse("<body></BODY>", G));
	assert(!lug::parse("<BODY></body>", G));
	assert(!lug::parse("<BODY></BODY2>", G));
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

	rule Xml, Inner, Name;

	Name = lexeme[alpha > *alnum];
	Xml = chr('<') > symbol("TAG")[Name] > chr('>') > ~Inner > str("</") > match("TAG") > chr('>');
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

void test_symbol_match_caseless()
{
	using namespace lug::language;

	rule Xml, Name;
	Name = lexeme[alpha > *alnum];
	Xml = chr('<') > Name > chr('>') > ~Xml > str("</") > caseless[match("TAG")] > chr('>');

	grammar G = start(Xml > eoi);

	environment E;
	assert(!E.has_symbol("TAG"));
	E.add_symbol("TAG", "a");
	assert(E.has_symbol("TAG"));

	assert(lug::parse("<a></a>", G, E));
	assert(lug::parse("<a></A>", G, E));
	assert(lug::parse("< a ></ a>", G, E));
	assert(lug::parse("< A ></ a>", G, E));
	assert(!lug::parse("<a></b>", G, E));
	assert(!lug::parse("<a></B>", G, E));

	assert(E.has_symbol("TAG"));
	E.clear_symbols("TAG");
	assert(!E.has_symbol("TAG"));
	E.add_symbol("TAG", "b");
	assert(E.has_symbol("TAG"));

	assert(lug::parse("<b></b>", G, E));
	assert(lug::parse("<b></B>", G, E));
	assert(lug::parse("<B></b>", G, E));
	assert(lug::parse("<b ></ b>", G, E));
	assert(!lug::parse("<b ></a>", G, E));
	assert(!lug::parse("<B ></a>", G, E));
	assert(!lug::parse("<b ></ a>", G, E));
	assert(!lug::parse("<b ></A>", G, E));

	assert(E.has_symbol("TAG"));
	E.add_symbol("TAG", "body");
	assert(E.has_symbol("TAG"));

	assert(lug::parse("<body></body>", G, E));
	assert(lug::parse("<body></BoDy>", G, E));
	assert(lug::parse("<body></BODY>", G, E));
	assert(lug::parse("<body></ body>", G, E));
	assert(lug::parse("<BODY></body>", G, E));
	assert(lug::parse("<BODY></bOdy>", G, E));
	assert(lug::parse("<BODY></BODY>", G, E));
	assert(lug::parse("< body></body >", G, E));
	assert(lug::parse("< boDy></BOdy >", G, E));
	assert(!lug::parse("<body></broken>", G, E));
	assert(!lug::parse("<body></BODY2>", G, E));
	assert(!lug::parse("<body></ broken>", G, E));
	assert(!lug::parse("<BODY></ broken>", G, E));
}

void test_symbol_definition_and_match_caseless()
{
	using namespace lug::language;

	rule Xml, Name;
	Name = lexeme[alpha > *alnum];
	Xml = chr('<') > symbol("TAG")[Name] > chr('>') > ~Xml > str("</") > caseless[match("TAG")] > chr('>');

	grammar G = start(Xml > eoi);

	assert(lug::parse("<a></a>", G));
	assert(lug::parse("<a></A>", G));
	assert(lug::parse("<A></a>", G));
	assert(lug::parse("<a></ a>", G));
	assert(lug::parse("< a></a>", G));
	assert(lug::parse("< A></a>", G));
	assert(lug::parse("< b></b>", G));
	assert(lug::parse("< b></B>", G));
	assert(lug::parse("<B></ b>", G));
	assert(lug::parse("<B></B>", G));
	assert(lug::parse("<b></ b>", G));
	assert(lug::parse("<b ></ b >", G));
	assert(lug::parse("< b ></b>", G));
	assert(lug::parse("<body></body>", G));
	assert(lug::parse("<body></ body>", G));
	assert(lug::parse("< body></body >", G));
	assert(lug::parse("<body></BODY>", G));
	assert(lug::parse("<BODY></body>", G));
	assert(lug::parse("<BODY></BODY>", G));
	assert(lug::parse("<body></BoDy>", G));

	assert(!lug::parse("<a></b>", G));
	assert(!lug::parse("<a></aa>", G));
	assert(!lug::parse("<aa></a>", G));
	assert(!lug::parse("<b></a>", G));
	assert(!lug::parse("< b></ a>", G));
	assert(!lug::parse("<b ></a>", G));
	assert(!lug::parse("<body></broken>", G));
	assert(!lug::parse("<body></ broken>", G));
	assert(!lug::parse("<body></body2>", G));
	assert(!lug::parse("<body></BODY2>", G));
}

int main()
{
	try {
		test_symbol_exists();
		test_symbol_match();
		test_symbol_definition_and_match();
		test_symbol_definition_and_match_2();
		test_symbol_definition_and_match_3();
		test_symbol_definition_and_match_any();
		test_symbol_definition_and_match_all();
		test_symbol_nested_definition_and_match();
		test_symbol_match_caseless();
		test_symbol_definition_and_match_caseless();
	} catch (std::exception const& e) {
		std::cerr << "Error: " << e.what() << "\n";
		return -1;
	} catch (...) {
		std::cerr << "Unknown Error\n";
		return -1;
	}
	return 0;
}
