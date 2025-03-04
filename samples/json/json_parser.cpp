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
using json_node_ptr = std::shared_ptr<json_node>;

// JSON node value types
using json_null = std::nullptr_t;
using json_bool = bool;
using json_number = double;
using json_string = std::string;
using json_array = std::vector<json_node_ptr>;
using json_object = std::map<std::string, json_node_ptr>;

// JSON node
struct json_node
{
	using value_type = std::variant
	<
		json_null,
		json_bool,
		json_number,
		json_string,
		json_array,
		json_object
	>;

	value_type value;

	json_node() = default;
	template <typename T> explicit json_node(T&& v) : value(std::forward<T>(v)) {}
	bool is_null() const { return std::holds_alternative<json_null>(value); }
	bool is_bool() const { return std::holds_alternative<json_bool>(value); }
	bool is_number() const { return std::holds_alternative<json_number>(value); }
	bool is_string() const { return std::holds_alternative<json_string>(value); }
	bool is_array() const { return std::holds_alternative<json_array>(value); }
	bool is_object() const { return std::holds_alternative<json_object>(value); }
	json_bool& as_bool() { return std::get<json_bool>(value); }
	json_number& as_number() { return std::get<json_number>(value); }
	json_string& as_string() { return std::get<json_string>(value); }
	json_array& as_array() { return std::get<json_array>(value); }
	json_object& as_object() { return std::get<json_object>(value); }
	json_bool const& as_bool() const { return std::get<json_bool>(value); }
	json_number const& as_number() const { return std::get<json_number>(value); }
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

		// JSON node factory functions
		auto MakeNull = [] { return std::make_shared<json_node>(nullptr); };
		auto MakeBool = [](syntax s) { return std::make_shared<json_node>(s.str() == "true"); };
		auto MakeNumber = [](syntax s) { return std::make_shared<json_node>(std::stod(std::string{s})); };

		auto MakeString = [](syntax s) {
			auto const str = s.str();
			return std::make_shared<json_node>(std::string{str.substr(1, str.size() - 2)});
		};

		auto MakeArray = [](environment& env) {
			return std::make_shared<json_node>(json_array{env.pop_attribute<json_node_ptr>()});
		};

		auto AppendArray = [](environment& env) {
			auto val{env.pop_attribute<json_node_ptr>()};
			env.top_attribute<json_node_ptr>()->as_array().push_back(std::move(val));
		};

		auto MakeObject = [](environment& env) {
			auto val{env.pop_attribute<json_node_ptr>()};
			auto name{env.pop_attribute<json_node_ptr>()};
			return std::make_shared<json_node>(json_object{{name->as_string(), std::move(val)}});
		};

		auto AppendObject = [](environment& env) {
			auto val{env.pop_attribute<json_node_ptr>()};
			auto name{env.pop_attribute<json_node_ptr>()};
			env.top_attribute<json_node_ptr>()->as_object()[name->as_string()] = std::move(val);
		};

		// JSON grammar rules
		rule JSON;
		auto ExponentPart   = lexeme[ "[Ee]"_rx > ~"[+-]"_rx > +"[0-9]"_rx ];
		auto FractionalPart = lexeme[ "."_sx > +"[0-9]"_rx ];
		auto IntegralPart   = lexeme[ "0"_sx | "[1-9]"_rx > *"[0-9]"_rx ];
		rule Number         = lexeme[ ~"-"_sx > IntegralPart > ~FractionalPart > ~ExponentPart ] < MakeNumber;
		rule Boolean        = lexeme[ "true"_sx | "false" ] < MakeBool;
		rule Null           = lexeme[ "null" ] < MakeNull;
		rule UnicodeEscape  = lexeme[ 'u' > "[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]"_rx ];
		rule Escape         = lexeme[ "\\" > ("[/\\bfnrt]"_rx | UnicodeEscape) ];
		rule String         = lexeme[ "\"" > *("[^\"\\\u0000-\u001F]"_rx | Escape) > "\"" ] < MakeString;
		rule Array          = '[' > JSON < MakeArray > *(',' > JSON < AppendArray) > ']';
		rule Object         = '{' > String > ':' > JSON < MakeObject > *(',' > String > ':' > JSON < AppendObject) > '}';
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
		const auto& array = node.as_array();
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
		const auto& object = node.as_object();
		std::size_t i = 0;
		for (const auto& [key, value] : object) {
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
