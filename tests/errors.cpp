// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2024 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>

#undef NDEBUG
#include <cassert>

using namespace std::string_view_literals;

// Test data with intentional errors
constexpr auto invalid_input =
R"(This input has {unclosed brackets
This line has mismatched (parentheses]}
This line has invalid @#$ symbols
This is an incomplete sentence that
)"sv;

void test_basic_error_recovery()
{
	using namespace lug::language;

	rule S, Sentence;

	// Define a simple grammar that can recover from errors
	Sentence = lexeme[+(!chr('\n') > any) > chr('\n')];
	S = *Sentence;

	grammar G = start(S > eoi);
	bool const result = lug::parse(invalid_input, G);
	assert(result); // Should complete despite errors
}

void test_basic_error_handling()
{
	using namespace lug::language;

	lug::syntax error_syntax;
	lug::syntax_position error_pos{0, 0};

	failure const FSymbolChar{"symbol character"};
	auto const Symbol = lexeme[+(!chr('@') > any) > (!chr('@'))[FSymbolChar]];

	rule S = Symbol > eoi ^= [&](error& e) -> error_response {
			assert(e.label() == "symbol character"sv);
			error_syntax = e.syntax();
			error_pos = e.position_begin();
			return error_response::halt;
		};

	grammar G = start(S);

	assert(!lug::parse("invalid@symbol", G));
	assert(error_syntax == "@symbol"sv);
	assert(error_pos.line == 1);
	assert(error_pos.column == 8);

	error_syntax = lug::syntax{};
	error_pos = lug::syntax_position{0, 0};
	assert(lug::parse("valid_symbol", G));
	assert(error_syntax.empty());
	assert(error_pos.line == 0);
	assert(error_pos.column == 0);
}

/*
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
}*/

int main()
{
	try {
		test_basic_error_recovery();
		test_basic_error_handling();
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
