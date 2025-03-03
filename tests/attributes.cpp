// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>
#include <sstream>

#undef NDEBUG
#include <cassert>

void test_addition_with_attributes()
{
	int lhs = 0; // left-hand side operand
	int rhs = 0; // right-hand side operand
	int result = 0; // variable to hold the result of the addition

	lug::grammar const G = [&]
	{
		using namespace lug::language;
		rule Number = lexeme[+"[0-9]"_rx] < [](std::string const& text) -> int { return std::stoi(text); };
		rule Addition = (lhs % Number) > '+' > (rhs % Number) < [&]{ result = lhs + rhs; };
		return start(Addition > eoi);
	}();

	std::string const text{"3+5"};
	assert(lug::parse(text, G));
	assert(result == 8);
	assert(lhs == 3);
	assert(rhs == 5);

	std::string const invalid{"4*2"};
	assert(!lug::parse(invalid, G));
	// Ensure the attributes were not modified when parsing failed
	assert(result == 8);
	assert(lhs == 3);
	assert(rhs == 5);
}

void test_nested_arithmetic_with_attributes()
{
	int lhs = 0; // left-hand side operand
	int rhs = 0; // right-hand side operand
	int result = 0; // variable to hold the final result

	lug::grammar const G = [&]
	{
		using namespace lug::language;
		rule Expression;
		auto Number = lexeme[+"[0-9]"_rx] <[](std::string const& text) -> int { return std::stoi(text); };
		auto Factor = (Number | '(' > Expression > ')');
		rule Term = (lhs%Factor) > *(('*' > (rhs%Factor) <[&]{ lhs *= rhs; })
								 | ('/' > (rhs%Factor) <[&]{ lhs /= rhs; }))     <[&]{ return lhs; };
		Expression = (lhs%Term) > *(('+' > (rhs%Term) <[&]{ lhs += rhs; })
								| ('-' > (rhs%Term) <[&]{ lhs -= rhs; }))        <[&]{ return lhs; };
		return start(result%Expression > eoi);
	}();

	std::string_view const arithmetic1{"4*(3+5)"};
	assert(lug::parse(arithmetic1, G));
	assert(result == 32);
	assert(lhs == 32);
	assert(rhs == 8);

	std::stringstream arithmetic2{"(9-(4+2)+0-(8*2))/2"};
	assert(lug::parse(arithmetic2, G));
	assert(result == -6);
	assert(lhs == -6);
	assert(rhs == 2);

	std::string const arithmetic3{"(3+5)*2"};
	assert(lug::parse(arithmetic3, G));
	assert(result == 16);
	assert(lhs == 16);
	assert(rhs == 2);

	std::string const invalid{"(3+*2"};
	assert(!lug::parse(invalid, G));
	// Ensure the result was not modified when parsing failed
	assert(result == 16);
	assert(lhs == 16);
	assert(rhs == 2);
}

int main()
{
	try {
		test_addition_with_attributes();
		test_nested_arithmetic_with_attributes();
	} catch (std::exception const& e) {
		std::cerr << "Error: " << e.what() << "\n";
		return -1;
	} catch (...) {
		std::cerr << "Unknown Error\n";
		return -1;
	}
	return 0;
}
