// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>
#include <iostream>

#undef NDEBUG
#include <cassert>

void test_empty()
{
	using namespace lug::language;
	rule S = noskip[ eps > eoi ];
	grammar G = start(S);
	assert(lug::parse("", G));
	assert(!lug::parse("a", G));
	assert(!lug::parse("2", G));
	assert(!lug::parse("z", G));
	assert(!lug::parse("aa", G));
	assert(!lug::parse("α", G)); // U+03B1 GREEK SMALL LETTER ALPHA
}

void test_eol()
{
	using namespace lug::language;
	rule S = noskip[ eol > eoi ];
	grammar G = start(S);

	// Test eol with different line endings
	assert(lug::parse("\n", G));
	assert(lug::parse("\r\n", G));
	assert(lug::parse("\r", G));
	assert(lug::parse("\f", G));
	assert(lug::parse("\v", G));
	assert(!lug::parse("\t", G));
	assert(!lug::parse("\b", G));
	assert(lug::parse("\u0085", G)); // U+0085 NEL Next Line
	assert(lug::parse("\u2028", G)); // U+2028 LS Line Separator
	assert(lug::parse("\u2029", G)); // U+2029 PS Paragraph Separator

	// Test token before eol
	assert(!lug::parse("a\n", G));
	assert(!lug::parse("a\r\n", G));
	assert(!lug::parse("a\r", G));
	assert(!lug::parse("a\f", G));
	assert(!lug::parse("a\v", G));
	assert(!lug::parse("a\t", G));
	assert(!lug::parse("a\b", G));
	assert(!lug::parse("a\u0085", G)); // U+0085 NEL Next Line
	assert(!lug::parse("a\u2028", G)); // U+2028 LS Line Separator
	assert(!lug::parse("a\u2029", G)); // U+2029 PS Paragraph Separator

	// Test space token before eol
	assert(!lug::parse(" \n", G));
	assert(!lug::parse(" \r\n", G));
	assert(!lug::parse(" \r", G));
	assert(!lug::parse(" \f", G));
	assert(!lug::parse(" \v", G));
	assert(!lug::parse(" \t", G));
	assert(!lug::parse(" \b", G));
	assert(!lug::parse(" \u0085", G)); // U+0085 NEL Next Line
	assert(!lug::parse(" \u2028", G)); // U+2028 LS Line Separator
	assert(!lug::parse(" \u2029", G)); // U+2029 PS Paragraph Separator

	// Test space token after eol
	assert(!lug::parse("\n ", G));
	assert(!lug::parse("\r\n ", G));
	assert(!lug::parse("\r ", G));
	assert(!lug::parse("\f ", G));
	assert(!lug::parse("\v ", G));
	assert(!lug::parse("\t ", G));
	assert(!lug::parse("\b ", G));
	assert(!lug::parse("\u0085 ", G)); // U+0085 NEL Next Line
	assert(!lug::parse("\u2028 ", G)); // U+2028 LS Line Separator
	assert(!lug::parse("\u2029 ", G)); // U+2029 PS Paragraph Separator
}

void test_any()
{
	using namespace lug::language;
	rule S = noskip[ any > eoi ];
	grammar G = start(S);
	assert(lug::parse("a", G));
	assert(lug::parse("2", G));
	assert(lug::parse("z", G));
	assert(lug::parse("α", G)); // U+03B1 GREEK SMALL LETTER ALPHA
	assert(!lug::parse("aa", G));
	assert(!lug::parse("", G));
	assert(!lug::parse(" a", G));
	assert(!lug::parse("a ", G));
	assert(!lug::parse("αa", G)); // U+03B1 GREEK SMALL LETTER ALPHA, FOLLOWED BY LETTER A
}

void test_char()
{
	using namespace lug::language;

	// ASCII character
	rule S = noskip[ chr('a') > eoi ];
	grammar G = start(S);
	assert(lug::parse("a", G));
	assert(!lug::parse("", G));
	assert(!lug::parse("2", G));
	assert(!lug::parse("aa", G));
	assert(!lug::parse("b", G));
	assert(!lug::parse(" a", G));

	// Unicode character
	rule S2 = noskip[ chr(U'\u03B1') > eoi ];
	grammar G2 = start(S2);
	assert(lug::parse("α", G2)); // U+03B1 GREEK SMALL LETTER ALPHA
	assert(!lug::parse("", G2));
	assert(!lug::parse("2", G2));
	assert(!lug::parse("aa", G2));
}

void test_char_range()
{
	using namespace lug::language;

	// ASCII character range
	rule S1 = noskip[ chr('d', 'g') > eoi ];
	grammar G1 = start(S1);
	assert(lug::parse("d", G1));
	assert(lug::parse("e", G1));
	assert(lug::parse("f", G1));
	assert(lug::parse("g", G1));
	assert(!lug::parse("", G1));
	assert(!lug::parse("c", G1));
	assert(!lug::parse("h", G1));
	assert(!lug::parse("dd", G1));
	assert(!lug::parse(" e", G1));
	assert(!lug::parse("f ", G1));
	assert(!lug::parse(" g ", G1));

	// ASCII single character range
	rule S2 = noskip[ chr('b', 'b') > eoi ];
	grammar G2 = start(S2);
	assert(lug::parse("b", G2));
	assert(!lug::parse("", G2));
	assert(!lug::parse("a", G2));
	assert(!lug::parse("c", G2));
	assert(!lug::parse("bb", G2));
	assert(!lug::parse(" b", G2));
	assert(!lug::parse("b ", G2));

	// Unicode range (Greek letters)
	rule S3 = noskip[ chr(U'\u03B1', U'\u03B5') > eoi ];
	grammar G3 = start(S3);
	assert(lug::parse("α", G3));  // U+03B1 GREEK SMALL LETTER ALPHA
	assert(lug::parse("β", G3));  // U+03B2 GREEK SMALL LETTER BETA
	assert(lug::parse("γ", G3));  // U+03B3 GREEK SMALL LETTER GAMMA
	assert(lug::parse("δ", G3));  // U+03B4 GREEK SMALL LETTER DELTA
	assert(lug::parse("ε", G3));  // U+03B5 GREEK SMALL LETTER EPSILON
	assert(!lug::parse("ζ", G3)); // U+03B6 GREEK SMALL LETTER ZETA
	assert(!lug::parse("α β", G3));
	assert(!lug::parse("", G3));
	assert(!lug::parse("a", G3));
	assert(!lug::parse(" α", G3));

	// Unicode single character range
	rule S4 = noskip[ chr(U'\u03B1', U'\u03B1') > eoi ];
	grammar G4 = start(S4);
	assert(lug::parse("α", G4));  // U+03B1 GREEK SMALL LETTER ALPHA
	assert(!lug::parse("", G4));
	assert(!lug::parse("a", G4));
	assert(!lug::parse("β", G4));
}

void test_string()
{
	using namespace lug::language;

	// ASCII string
	rule S = noskip[ str("hello world") > eoi ];
	grammar G = start(S);
	assert(lug::parse("hello world", G));
	assert(!lug::parse("hello world!", G));
	assert(!lug::parse("hello", G));
	assert(!lug::parse("h", G));
	assert(!lug::parse("", G));

	// Unicode string
	rule S2 = noskip[ str("καλημέρα κόσμε") > eoi ];
	grammar G2 = start(S2);
	assert(lug::parse("καλημέρα κόσμε", G2));
	assert(!lug::parse("καλημέρα κόσμε!", G2));
	assert(!lug::parse("hello world", G2));
	assert(!lug::parse("h", G2));
}

void test_regex_simple()
{
	using namespace lug::language;

	// ASCII regular expression
	rule S = noskip[ bre("hello.w[oO]rld[[:digit:]]") > eoi ];
	grammar G = start(S);
	assert(lug::parse("hello world4", G));
	assert(lug::parse("hello_wOrld8", G));
	assert(!lug::parse("hello world!", G));
	assert(!lug::parse("hello", G));
	assert(!lug::parse("h", G));
	assert(!lug::parse("", G));

	// Unicode regular expression
	rule S2 = noskip[ bre("κα[λΛ]ημέρα κ[όΟ]σμε") > eoi ];
	grammar G2 = start(S2);
	assert(lug::parse("καΛημέρα κόσμε", G2));
	assert(lug::parse("καλημέρα κΟσμε", G2));
	assert(!lug::parse("καλημέρα κόσμε!", G2));
	assert(!lug::parse("hello world", G2));
	assert(!lug::parse("h", G2));

	// Unicode negated regular expression with escape sequences
	rule S3 = noskip[ +"[^\"\\\u0000-\u001F]"_rx > eoi ];
	grammar G3 = start(S3);
	assert(lug::parse("Hello, world!🌍", G3));
	assert(lug::parse("Hello, 世界!🌍", G3));
	assert(lug::parse("Hello, moon!🌕", G3));
	assert(lug::parse("Hello, 月!🌕", G3));
	assert(!lug::parse("\"hello world\"", G3));
	assert(!lug::parse("\\hello world", G3));
	assert(!lug::parse("\u0000hello world", G3));
	assert(!lug::parse("\u001Fhello world", G3));

	rule S4 = noskip[ +"[^\"\\\u0000-\u001F\U0001F315]"_rx > eoi ];
	grammar G4 = start(S4);
	assert(lug::parse("Hello, world!🌍", G4));
	assert(lug::parse("Hello, 世界!🌍", G4));
	assert(!lug::parse("Hello, moon!🌕", G4));
	assert(!lug::parse("Hello, 月!🌕", G4));
	assert(!lug::parse("\"hello world\"", G4));
	assert(!lug::parse("\\hello world", G4));
	assert(!lug::parse("\u0000hello world", G4));
	assert(!lug::parse("\u001Fhello world", G4));
}

/* TODO: Implement full BRE support
void test_regex_complex()
{
	using namespace lug::language;
	
	// Test nested groups and quantifiers
	rule S1 = noskip[ bre("(abb*c?)*") > eoi ];
	grammar G1 = start(S1);
	assert(lug::parse("", G1));
	assert(lug::parse("ab", G1));
	assert(lug::parse("abbc", G1));
	assert(lug::parse("ababc", G1));
	assert(!lug::parse("ac", G1));
}*/

void test_regex_unicode_categories()
{
	using namespace lug::language;
	
	// Test unicode letter category
	rule S1 = noskip[ bre("[[:alpha:]]") > eoi ];
	grammar G1 = start(S1);
	assert(lug::parse("a", G1));
	assert(lug::parse("α", G1));  // Greek alpha
	assert(!lug::parse("1", G1));
	assert(!lug::parse("", G1));
	
	// Test unicode number category
	rule S2 = noskip[ bre("[[:digit:]]") > eoi ];
	grammar G2 = start(S2);
	assert(lug::parse("1", G2));
	assert(lug::parse("٣", G2));  // Arabic-Indic digit three
	assert(!lug::parse("a", G2));
	assert(!lug::parse("", G2));
}

void test_character_classes()
{
	using namespace lug::language;
	
	// Test digit character class
	rule S1 = noskip[ digit > eoi ];
	grammar G1 = start(S1);
	assert(lug::parse("0", G1));
	assert(lug::parse("9", G1));
	assert(!lug::parse("a", G1));
	assert(!lug::parse("", G1));
	
	// Test alpha character class
	rule S2 = noskip[ alpha > eoi ];
	grammar G2 = start(S2);
	assert(lug::parse("a", G2));
	assert(lug::parse("Z", G2));
	assert(!lug::parse("1", G2));
	assert(!lug::parse("", G2));
	
	// Test alnum character class
	rule S3 = noskip[ alnum > eoi ];
	grammar G3 = start(S3);
	assert(lug::parse("a", G3));
	assert(lug::parse("Z", G3));
	assert(lug::parse("1", G3));
	assert(!lug::parse("@", G3));
	
	// Test space character class
	rule S4 = noskip[ space > eoi ];
	grammar G4 = start(S4);
	assert(lug::parse(" ", G4));
	assert(lug::parse("\t", G4));
	assert(!lug::parse("a", G4));

	// Test xdigit character class
	rule S5 = noskip[ xdigit > eoi ];
	grammar G5 = start(S5);
	assert(lug::parse("0", G5));
	assert(lug::parse("9", G5));
	assert(lug::parse("a", G5));
	assert(lug::parse("F", G5));
	assert(!lug::parse("g", G5));
	assert(!lug::parse("G", G5));
	assert(!lug::parse("", G5));
	
	// Test punct character class
	rule S6 = noskip[ punct > eoi ];
	grammar G6 = start(S6);
	assert(lug::parse(".", G6));
	assert(lug::parse(",", G6));
	assert(lug::parse("!", G6));
	assert(lug::parse("?", G6));
	assert(!lug::parse("a", G6));
	assert(!lug::parse("1", G6));
	assert(!lug::parse(" ", G6));
	assert(!lug::parse("", G6));
	
	// Test graph character class
	rule S7 = noskip[ graph > eoi ];
	grammar G7 = start(S7);
	assert(lug::parse("a", G7));
	assert(lug::parse("1", G7));
	assert(lug::parse("!", G7));
	assert(!lug::parse(" ", G7));
	assert(!lug::parse("\t", G7));
	assert(!lug::parse("", G7));
	
	// Test print character class
	rule S8 = noskip[ print > eoi ];
	grammar G8 = start(S8);
	assert(lug::parse("a", G8));
	assert(lug::parse("1", G8));
	assert(lug::parse("!", G8));
	assert(lug::parse(" ", G8));
	assert(!lug::parse("\t", G8));
	assert(!lug::parse("", G8));
}

int main()
try {
	test_empty();
	test_any();
	test_eol();
	test_char();
	test_char_range();
	test_string();
	test_regex_simple();
	/*test_regex_complex();*/
	test_regex_unicode_categories();
	test_character_classes();
	return 0;
} catch (std::exception const& e) {
	std::cerr << "Error: " << e.what() << "\n";
	return 1;
} catch (...) {
	std::cerr << "Unknown Error\n";
	return 1;
}
