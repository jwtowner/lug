// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>
#include <iostream>

#undef NDEBUG
#include <cassert>

void test_directives_stack_no_skip_operations()
{
	lug::program prog;
	lug::program_callees prog_callees;
	for (int i = 0; i < 256; ++i) {
		auto const initial_mode = static_cast<lug::directives>(static_cast<std::uint_least8_t>(i));
		lug::encoder enc{prog, prog_callees, initial_mode};
		enc.dpsh(lug::directives::none, lug::directives::none);
		assert(enc.mode() == initial_mode); // No change
		enc.dpop(lug::directives::none);
		assert(enc.mode() == initial_mode); // No change
		assert(prog.instructions.empty()); // No instructions added
	}
}

int main()
try {
	test_directives_stack_no_skip_operations();
	return 0;
} catch (std::exception const& e) {
	std::cerr << "Error: " << e.what() << "\n";
	return 1;
} catch (...) {
	std::cerr << "Unknown Error\n";
	return 1;
}
