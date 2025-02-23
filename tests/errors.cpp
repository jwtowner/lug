// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2024 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>

#undef NDEBUG
#include <cassert>

using namespace std::string_view_literals;
using namespace lug::language;

// Test data with intentional errors
constexpr auto invalid_input =
R"(This input has {unclosed brackets
This line has mismatched (parentheses]}
This line has invalid @#$ symbols
This is an incomplete sentence that
)"sv;

void test_basic_error_recovery()
{
	rule S, Sentence;

	// Define a simple grammar that can recover from errors
	Sentence = lexeme[+(!chr('\n') > any) > chr('\n')];
	S = *Sentence;

	environment E;
	grammar G = start(S > eoi);

	// Should be able to continue parsing after errors
	bool const result = lug::parse(invalid_input, G, E);
	assert(result); // Should complete despite errors
}

void test_error_position_tracking()
{
	rule S;
	std::array<lug::syntax_position, 2> error_pos;
	error_pos.fill({0, 0});

	failure f_at{"@"};

	//S = (+(~chr('@')) > chr('@'))[f_at] ^[&](error& e) -> error_response { };

	S = lexeme[+(!chr('@') > any) > chr('@')] <[&](environment& e, syntax x) {
		error_pos[0] = e.position_begin(x);
	};

	environment E;
	grammar G = start(S > eoi);

	bool const result = lug::parse("invalid@symbol", G, E);
	assert(!result);
	assert(error_pos[0].line > 0); // Should have tracked error position
}

void test_custom_error_handlers()
{
	rule S, ErrorRecovery;
	bool error_handler_called = false;

	// Define error recovery rule
	ErrorRecovery = lexeme[*(~chr('\n'))] > chr('\n') < [&](environment&, syntax) {
		error_handler_called = true;
	};

	S = (lexeme[+(~chr('\n'))] | ErrorRecovery) > chr('\n');

	environment E;
	grammar G = start(S > eoi);

	bool const result = lug::parse("This line has an error!\n", G, E);
	assert(!result);
	assert(error_handler_called);
}

void test_nested_recovery()
{
	rule S, Block;
	int recovery_count = 0;

	// Test nested structure recovery
	Block = chr('{') > lexeme[*(~chr('}'))] > chr('}') < [&](environment&, syntax) {
		recovery_count++;
	};

	S = *Block;

	environment E;
	grammar G = start(S > eoi);

	bool const result = lug::parse("{valid} {invalid", G, E);
	assert(!result);
	assert(recovery_count == 1); // Should have attempted recovery once
}

int main()
{
	try {
		test_basic_error_recovery();
		//test_error_position_tracking();
		//test_custom_error_handlers();
		//test_nested_recovery();
		return 0;
	} catch (std::exception const& e) {
		std::cerr << "Error: " << e.what() << "\n";
		return -1;
	} catch (...) {
		std::cerr << "Error: Unknown exception\n";
		return -1;
	}
}
