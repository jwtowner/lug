// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#include "json_matcher.hpp"

#include <cstdlib>
#include <fstream>

// Command line options
struct options
{
	std::string filename{"-"};
	bool quiet{false};
};

// Prints verbose output
struct verbose_cout
{
	options const& opts;
	
	template <typename T>
	friend verbose_cout&& operator<<(verbose_cout&& os, T&& v)
	{
		if (!os.opts.quiet)
			std::cout << std::forward<T>(v);
		return static_cast<verbose_cout&&>(os);
	}
};

// Prints usage information
void print_usage()
{
	std::cout << "Usage: jsonformat [options] [file|-]\n"
	          << "Options:\n"
	          << "  -q, --quiet       Only set exit code\n"
	          << "  -h, --help        Show this help\n"
	          << "If no file is specified, reads from stdin.\n";
}

// Parses command line arguments
options parse_args(int argc, char* argv[]) {
	options opts;
	for (int i = 1; i < argc; ++i) {
		std::string_view const arg{argv[i]};
		if (arg == "-h" || arg == "--help") {
			print_usage();
			std::exit(EXIT_SUCCESS);
		} else if (arg == "-q" || arg == "--quiet") {
			opts.quiet = true;
		} else if ((arg.size() > 1) && (arg.front() == '-')) {
			throw std::runtime_error("Unknown option: " + std::string{arg});
		} else {
			opts.filename = arg;
		}
	}
	return opts;
}

int main(int argc, char* argv[])
try {
	auto const opts = parse_args(argc, argv);
	auto const matched = [&] {
		if (opts.filename == "-")
			return json_matcher{}.match_cin();
		std::ifstream input_file;
		input_file.open(opts.filename);
		if (!input_file.is_open())
			throw std::runtime_error("Failed to open file: " + opts.filename);
		return json_matcher{}.match(input_file);
	}();
	if (!matched) {
		verbose_cout{opts} << "Invalid JSON\n";
		return 1;
	}
	verbose_cout{opts} << "Valid JSON\n";
	return 0;
} catch (std::exception const& e) {
	std::cerr << "ERROR: " << e.what() << "\n";
	return 1;
} catch (...) {
	std::cerr << "UNKNOWN ERROR\n";
	return 1;
}
