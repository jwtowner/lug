// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#include "json_parser.hpp"

#include <cstdlib>
#include <fstream>

// Command line options
struct options
{
	std::string filename{"-"};
	int indent{2};
	bool compact{false};
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
options parse_options(int argc, char* argv[])
{
	options opts;
	for (int i = 1; i < argc; ++i) {
		std::string_view const arg{argv[i]};
		if (arg == "-h" || arg == "--help") {
			print_usage();
			std::exit(EXIT_SUCCESS);
		} else if (arg == "-c" || arg == "--compact") {
			opts.compact = true;
		} else if (arg == "-q" || arg == "--quiet") {
			opts.quiet = true;
		} else if (arg == "-i" || arg == "--indent") {
			if ((i + 1) < argc) {
				opts.indent = std::stoi(argv[++i]);
			} else {
				throw std::runtime_error("Missing argument for option: " + std::string{arg});
			}
		} else if ((arg.size() > 1) && (arg.front() == '-')) {
			throw std::runtime_error("Unknown option: " + std::string{arg});
		} else {
			opts.filename = arg;
		}
	}
	return opts;
}

// Recursively writes a JSON node tree to an output stream
void write_json(std::ostream& os, json_node const& node, int indent = 0, bool pretty = true)
{
	std::string const indentation(static_cast<std::size_t>(pretty ? indent : 0), ' ');
	std::string_view const newline{pretty ? "\n" : ""};
	if (node.is_null()) {
		os << "null";
	} else if (node.is_bool()) {
		os << (node.as_bool() ? "true" : "false");
	} else if (node.is_number()) {
		os << node.as_number();
	} else if (node.is_string()) {
		os << "\"" << node.as_string() << "\"";
	} else if (node.is_array()) {
		os << "[" << newline;
		auto const& array = node.as_array();
		for (std::size_t i = 0; i < array.size(); ++i) {
			if (pretty)
				os << indentation << "  ";
			write_json(os, *array[i], indent + 2, pretty);
			if (i < array.size() - 1)
				os << ",";
			os << newline;
		}
		os << indentation << "]";
	} else if (node.is_object()) {
		os << "{" << newline;
		auto const& object = node.as_object();
		std::size_t i = 0;
		for (auto const& [key, value] : object) {
			if (pretty)
				os << indentation << "  ";
			os << "\"" << key << "\":";
			if (pretty)
				os << " ";
			write_json(os, *value, indent + 2, pretty);
			if (i < object.size() - 1)
				os << ",";
			os << newline;
			++i;
		}
		os << indentation << "}";
	}
}

int main(int argc, char* argv[])
try {
	auto const opts = parse_options(argc, argv);
	auto const json = [&] {
		if (opts.filename == "-")
			return json_parser{}.parse_cin();
		std::ifstream input_file;
		input_file.open(opts.filename);
		if (!input_file.is_open())
			throw std::runtime_error("Failed to open file: " + opts.filename);
		return json_parser{}.parse(input_file);
	}();
	if (!json) {
		verbose_cout{opts} << "Invalid JSON\n";
		return 1;
	}
	write_json(std::cout, *json, 0, !opts.compact);
	std::cout << std::endl;
	return 0;
} catch (std::exception const& e) {
	std::cerr << "ERROR: " << e.what() << "\n";
	return -1;
} catch (...) {
	std::cerr << "UNKNOWN ERROR\n";
	return -1;
}
