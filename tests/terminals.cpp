// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>

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

void test_regular_expression()
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
	rule S3 = noskip[ +"[^\"\\\u0000-\u001F\U0001F315]"_rx > eoi ];
	grammar G3 = start(S3);
	assert(lug::parse("Hello, world!🌍", G3));
	assert(lug::parse("Hello, 世界!🌍", G3));
	assert(!lug::parse("Hello, moon!🌕", G3));
	assert(!lug::parse("Hello, 月!🌕", G3));
	assert(!lug::parse("\"hello world\"", G3));
	assert(!lug::parse("\\hello world", G3));
	assert(!lug::parse("\u0000hello world", G3));
	assert(!lug::parse("\u001Fhello world", G3));
}

int main()
{
	try {
		test_empty();
		test_any();
		test_char();
		test_char_range();
		test_string();
		test_regular_expression();
	} catch (std::exception const& e) {
		std::cerr << "Error: " << e.what() << "\n";
		return -1;
	} catch (...) {
		std::cerr << "Unknown Error\n";
		return -1;
	}
	return 0;
}
