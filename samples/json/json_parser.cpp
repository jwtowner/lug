// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>

#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <variant>
#include <vector>

// JSON node pointer type
struct json_node;
using json_node_ptr = std::unique_ptr<json_node>;

// JSON node value types
using json_null = std::monostate;
using json_bool = bool;
using json_number = double;
using json_string = std::string;
using json_array = std::vector<json_node_ptr>;
using json_object = std::map<std::string, json_node_ptr>;

// JSON node
struct json_node
{
	std::variant<json_null, json_bool, json_number, json_string, json_array, json_object> value;

	json_node() = default;
	template <typename T> explicit json_node(T&& v) : value(std::forward<T>(v)) {}

	bool is_null() const { return std::holds_alternative<json_null>(value); }
	bool is_bool() const { return std::holds_alternative<json_bool>(value); }
	bool is_number() const { return std::holds_alternative<json_number>(value); }
	bool is_string() const { return std::holds_alternative<json_string>(value); }
	bool is_array() const { return std::holds_alternative<json_array>(value); }
	bool is_object() const { return std::holds_alternative<json_object>(value); }

	json_bool as_bool() const { return std::get<json_bool>(value); }
	json_number as_number() const { return std::get<json_number>(value); }
	json_string const& as_string() const { return std::get<json_string>(value); }
	json_array const& as_array() const { return std::get<json_array>(value); }
	json_object const& as_object() const { return std::get<json_object>(value); }
};

// Parser for JSON Data Interchange Standard (RFC7159)
class json_parser
{
public:
	json_parser()
	{
		using namespace lug::language;

		// JSON node value factory functions
		auto MakeNull = []{ return std::make_unique<json_node>(json_null{}); };
		auto MakeBool = [](std::string_view s) { return std::make_unique<json_node>(s == "true"); };
		auto MakeNumber = [](std::string_view s) { return std::make_unique<json_node>(std::stod(std::string{s})); };
		auto MakeKeyOrString = [](std::string_view s) { return std::string{s.substr(1, s.size() - 2)}; };

		// JSON grammar rules
		rule JSON;
		rule ExponentPart   = lexeme[ "[Ee]"_rx > ~"[+-]"_rx > +"[0-9]"_rx ];
		rule FractionalPart = lexeme[ "."_sx > +"[0-9]"_rx ];
		rule IntegralPart   = lexeme[ "0"_sx | "[1-9]"_rx > *"[0-9]"_rx ];
		rule Number         = lexeme[ ~"-"_sx > IntegralPart > ~FractionalPart > ~ExponentPart ] < MakeNumber;
		rule Boolean        = lexeme[ "true"_sx | "false" ] < MakeBool;
		rule Null           = lexeme[ "null" ] < MakeNull;
		rule UnicodeEscape  = lexeme[ 'u' > "[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]"_rx ];
		rule Escape         = lexeme[ '\\' > ("[/\\bfnrt]"_rx | UnicodeEscape) ];
		rule KeyOrString    = lexeme[ '"' > *("[^\"\\\u0000-\u001F]"_rx | Escape) > '"' ] < MakeKeyOrString;
		rule String         = synthesize_unique<json_node, std::string>[ KeyOrString ];
		rule Array          = '[' > synthesize_unique<json_node, json_array>[ collect<json_array>[ JSON >> ',' ] ] > ']';
		rule Object         = '{' > synthesize_unique<json_node, json_object>[ collect<json_object, std::string, json_node_ptr>[ ( KeyOrString > ':' > JSON ) >> ',' ] ] > '}';
		JSON                = Null | Boolean | Number | String | Array | Object;
		grammar_            = start(JSON > eoi);
	}

	bool parse_cin()
	{
		return lug::parse(grammar_, environment_);
	}

	json_node_ptr const& value() const
	{
		return environment_.top_attribute<json_node_ptr>();
	}

private:
	lug::grammar grammar_;
	lug::environment environment_;
};

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
			os << "\"" << key << "\": ";
			write_json(os, *value, indent + 2, pretty);
			if (i < object.size() - 1)
				os << ",";
			os << newline;
			++i;
		}
		os << indentation << "}";
	}
}

int main()
{
	try {
		json_parser reader;
		if (!reader.parse_cin()) {
			std::cout << "Invalid JSON!\n";
			return -1;
		}
		write_json(std::cout, *reader.value());
		std::cout << std::endl;
	} catch (std::exception const& e) {
		std::cerr << "ERROR: " << e.what() << "\n";
		return -1;
	} catch (...) {
		std::cerr << "UNKNOWN ERROR\n";
		return -1;
	}
	return 0;
}
