// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017 Jesse W. Towner
// See LICENSE.md file for license details

#ifdef _MSC_VER
#define _SILENCE_PARALLEL_ALGORITHMS_EXPERIMENTAL_WARNING
#endif

#include <cassert>
#include <cctype>
#include <cmath>
#include <cstdint>
#include <algorithm>
#include <array>
#include <execution>
#include <fstream>
#include <future>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <limits>
#include <map>
#include <regex>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#if __has_include(<filesystem>)
#include <filesystem>
#if defined __cpp_lib_filesystem && __cpp_lib_filesystem >= 201603
namespace stdfs = std::filesystem;
#else
namespace stdfs = std::experimental::filesystem;
#endif
#elif __has_include(<experimental/filesystem>)
#include <experimental/filesystem>
namespace stdfs = std::experimental::filesystem;
#else
#error "Standard C++ filesystem library is required"
#endif

template <class Integer>
class integer_iterator
{
public:
	typedef Integer value_type;
	typedef value_type const& reference;
	typedef value_type const* pointer;
	typedef std::common_type_t<std::ptrdiff_t, std::make_signed_t<Integer>> difference_type;
	typedef std::random_access_iterator_tag iterator_category;
	constexpr integer_iterator() noexcept : value_{0} {};
	constexpr explicit integer_iterator(Integer value) : value_{value} {}
	reference operator*() const { return value_; }
	value_type operator[](difference_type n) const { return static_cast<value_type>(value_ + n); }
	integer_iterator& operator++() { ++value_; return *this; }
	integer_iterator& operator--() { --value_; return *this; }
	integer_iterator operator++(int) { integer_iterator i{value_}; ++value_; return i; }
	integer_iterator operator--(int) { integer_iterator i{value_}; --value_; return i; }
	integer_iterator& operator+=(difference_type n) { value_ = static_cast<value_type>(value_ + n); return *this; }
	integer_iterator& operator-=(difference_type n) { value_ = static_cast<value_type>(value_ - n); return *this; }
	friend constexpr bool operator==(integer_iterator const& x, integer_iterator const& y) noexcept { return x.value_ == y.value_; }
	friend constexpr bool operator!=(integer_iterator const& x, integer_iterator const& y) noexcept { return x.value_ != y.value_; }
	friend constexpr bool operator<(integer_iterator const& x, integer_iterator const& y) noexcept { return x.value_ < y.value_; }
	friend constexpr bool operator<=(integer_iterator const& x, integer_iterator const& y) noexcept { return x.value_ <= y.value_; }
	friend constexpr bool operator>(integer_iterator const& x, integer_iterator const& y) noexcept { return x.value_ > y.value_; }
	friend constexpr bool operator>=(integer_iterator const& x, integer_iterator const& y) noexcept { return x.value_ >= y.value_; }
	friend constexpr integer_iterator operator+(integer_iterator const& x, difference_type n) noexcept { return integer_iterator{static_cast<value_type>(x.value_ + n)}; }
	friend constexpr integer_iterator operator+(difference_type n, integer_iterator const& x) noexcept { return integer_iterator{static_cast<value_type>(x.value_ + n)}; }
	friend constexpr integer_iterator operator-(integer_iterator const& x, difference_type n) noexcept { return integer_iterator{static_cast<value_type>(x.value_ - n)}; }
	friend constexpr difference_type operator-(integer_iterator const& x, integer_iterator const& y) noexcept { return static_cast<difference_type>(x.value_) - static_cast<difference_type>(y.value_); }
private:
	Integer value_;
};

template <class Integer>
constexpr integer_iterator<Integer> make_integer_iterator(Integer value) noexcept
{
	return integer_iterator<Integer>{value};
}

constexpr std::size_t hash_combine(std::size_t x, std::size_t y) noexcept
{
	return x + 0x9e3779b9 + (y << 6) + (y >> 2);
}

template <class... Sizes>
constexpr std::size_t hash_combine(std::size_t x, std::size_t y, Sizes... z) noexcept
{
	return hash_combine(hash_combine(x, y), z...);
}

class makeunicode_error : public std::runtime_error
{
	using std::runtime_error::runtime_error;
};

enum class enum_type { bitfield, index };

template <enum_type type, class Integral, class List>
auto build_namemap(List const& namelist)
{
	if constexpr (type == enum_type::bitfield)
		assert(namelist.size() <= std::numeric_limits<Integral>::digits);
	else
		assert(namelist.size() <= (std::numeric_limits<Integral>::max)());
	std::unordered_map<std::string, Integral> namemap;
	namemap.reserve(namelist.size());
	for (std::size_t i = 0, n = namelist.size(); i < n; ++i) {
		if constexpr (type == enum_type::bitfield)
			namemap.emplace(namelist[i], static_cast<Integral>(Integral{1} << i));
		else
			namemap.emplace(namelist[i], static_cast<Integral>(i));
	}
	return namemap;
}

static std::vector<std::string> const binary_property_names =
{
	// Custom names
	"Any", "Ascii", "Assigned",
	// PropList.txt
	"White_Space", "Bidi_Control", "Join_Control", "Dash", "Quotation_Mark", "Terminal_Punctuation",
	"Other_Math", "Hex_Digit", "ASCII_Hex_Digit", "Other_Alphabetic", "Ideographic", "Diacritic", "Extender",
	"Other_Lowercase", "Other_Uppercase", "Noncharacter_Code_Point", "Other_Grapheme_Extend",
	"IDS_Binary_Operator", "IDS_Ternary_Operator", "Radical", "Unified_Ideograph",
	"Other_Default_Ignorable_Code_Point", "Soft_Dotted", "Logical_Order_Exception", "Other_ID_Start",
	"Other_ID_Continue", "Sentence_Terminal", "Variation_Selector", "Pattern_White_Space", "Pattern_Syntax",
	"Prepended_Concatenation_Mark", "Regional_Indicator",
	// DerivedBinaryProperties.txt
	"Lowercase", "Uppercase", "Cased", "Case_Ignorable", "Changes_When_Lowercased", "Changes_When_Uppercased",
	"Changes_When_Titlecased", "Changes_When_Casefolded", "Changes_When_Casemapped", "Alphabetic",
	"Default_Ignorable_Code_Point", "Grapheme_Base", "Grapheme_Extend", "Math", "ID_Start", "ID_Continue",
	"XID_Start", "XID_Continue"
};

static auto const binary_properties = build_namemap<enum_type::bitfield, std::uint_least64_t>(binary_property_names);

static std::vector<std::string> const general_category_names =
{
	"Ll", "Lm", "Lo", "Lt", "Lu", "Mc", "Me", "Mn", "Nd", "Nl", "No", "Pc", "Pd", "Pe", "Pf",
	"Pi", "Po", "Ps", "Sc", "Sk", "Sm", "So", "Zl", "Zp", "Zs", "Cc", "Cf", "Cn", "Co", "Cs"
};

static std::vector<std::string> const general_category_long_names =
{
	"Lowercase_Letter", "Modifier_Letter", "Other_Letter", "Titlecase_Letter", "Uppercase_Letter",
	"Spacing_Mark", "Enclosing_Mark", "Nonspacing_Mark",
	"Decimal_Number", "Letter_Number", "Other_Number",
	"Connector_Punctuation", "Dash_Punctuation", "Close_Punctuation", "Final_Punctuation",
	"Initial_Punctuation", "Other_Punctuation", "Open_Punctuation",
	"Currency_Symbol", "Modifier_Symbol", "Mathematical_Symbol", "Other_Symbol",
	"Line_Separator", "Paragraph_Separator", "Space_Separator",
	"Control", "Format", "Unassigned", "Private_Use", "Surrogate"
};

static std::map<std::string, std::pair<std::string_view, std::vector<std::string_view>>> const compound_general_categories =
{
	{ "LC", { "Cased_Letter", { "Lu", "Ll", "Lt" } } },
	{ "L", { "Letter", { "Lu", "Ll", "Lt", "Lm", "Lo" } } },
	{ "M", { "Mark", { "Mc", "Me", "Mn" } } },
	{ "N", { "Number", { "Nd", "Nl", "No" } } },
	{ "P", { "Punctuation", { "Pc", "Pd", "Pe", "Pf", "Pi", "Po", "Ps" } } },
	{ "S", { "Symbol", { "Sc", "Sm", "Sk", "So" } } },
	{ "Z", { "Separator", { "Zl", "Zp", "Zs" } } },
	{ "C", { "Other", { "Cc", "Cf", "Cn", "Co", "Cs" } } }
};

static auto const general_categories = build_namemap<enum_type::index, std::uint_least8_t>(general_category_names);

static std::vector<std::string> const compatibility_property_names =
{
	"alpha", "lower", "upper", "punct", "digit", "xdigit", "alnum", "space", "blank", "cntrl", "graph", "print", "word"
};

static auto const compatibility_properties = build_namemap<enum_type::bitfield, std::uint_least16_t>(compatibility_property_names);

static std::vector<std::string> const script_names =
{
	"Unknown", "Common", "Inherited", "Arabic", "Armenian", "Bengali", "Bopomofo", "Braille", "Buginese", "Buhid",
	"Canadian_Aboriginal", "Cherokee", "Coptic", "Cypriot", "Cyrillic", "Deseret", "Devanagari", "Ethiopic", "Georgian",
	"Glagolitic", "Gothic", "Greek", "Gujarati", "Gurmukhi", "Han", "Hangul", "Hanunoo", "Hebrew", "Hiragana", "Kannada",
	"Katakana", "Kharoshthi", "Khmer", "Lao", "Latin", "Limbu", "Linear_B", "Malayalam", "Mongolian", "Myanmar",
	"New_Tai_Lue", "Ogham", "Old_Italic", "Old_Persian", "Oriya", "Osmanya", "Runic", "Shavian", "Sinhala", "Syloti_Nagri",
	"Syriac", "Tagalog", "Tagbanwa", "Tai_Le", "Tamil", "Telugu", "Thaana", "Thai", "Tibetan", "Tifinagh", "Ugaritic", "Yi",
	// Unicode 5.0
	"Balinese", "Cuneiform", "Nko", "Phags_Pa", "Phoenician",
	// Unicode 5.1
	"Carian", "Cham", "Kayah_Li", "Lepcha", "Lycian", "Lydian", "Ol_Chiki", "Rejang", "Saurashtra", "Sundanese", "Vai",
	// Unicode 5.2
	"Avestan", "Bamum", "Egyptian_Hieroglyphs", "Imperial_Aramaic", "Inscriptional_Pahlavi", "Inscriptional_Parthian",
	"Javanese", "Kaithi", "Lisu", "Meetei_Mayek", "Old_South_Arabian", "Old_Turkic", "Samaritan", "Tai_Tham", "Tai_Viet",
	// Unicode 6.0.0
	"Batak", "Brahmi", "Mandaic",
	// Unicode 6.1.0
	"Chakma", "Meroitic_Cursive", "Meroitic_Hieroglyphs", "Miao", "Sharada", "Sora_Sompeng", "Takri",
	// Unicode 7.0.0
	"Bassa_Vah", "Caucasian_Albanian", "Duployan", "Elbasan", "Grantha", "Khojki", "Khudawadi", "Linear_A", "Mahajani",
	"Manichaean", "Mende_Kikakui", "Modi", "Mro", "Nabataean", "Old_North_Arabian", "Old_Permic", "Pahawh_Hmong",
	"Palmyrene", "Psalter_Pahlavi", "Pau_Cin_Hau", "Siddham", "Tirhuta", "Warang_Citi",
	// Unicode 8.0.0
	"Ahom", "Anatolian_Hieroglyphs", "Hatran", "Multani", "Old_Hungarian", "SignWriting",
	// Unicode 10.0.0
	"Adlam", "Bhaiksuki", "Marchen", "Newa", "Osage", "Tangut", "Masaram_Gondi", "Nushu", "Soyombo", "Zanabazar_Square"
};

static auto const scripts = build_namemap<enum_type::index, std::uint_least8_t>(script_names);

template <class T> using ucd_array = std::array<T, 0x110000>;
static ucd_array<std::uint_least64_t> ptable; // Binary Properties table
static ucd_array<std::uint_least16_t> ctable; // POSIX Compatibility table
static ucd_array<std::uint_least8_t> gctable; // General Category table
static ucd_array<std::uint_least8_t> sctable; // Script table

constexpr auto default_rx_options = std::regex::ECMAScript | std::regex::optimize;
static std::regex const rx_ucd_version(R"(^#\s*\w+-(\d+(\.\d+)+).*)", default_rx_options);
static std::regex const rx_ucd_prop_range(R"(^\s*([0-9a-fA-F]+)(\.\.([0-9a-fA-F]+))?\s*;\s*(\w+)\s*.*)", default_rx_options);

std::ifstream open_ucd_file_and_read_version(stdfs::path const& filepath, std::string& version)
{
	std::ifstream input{filepath};
	if (!input)
		throw makeunicode_error("unable to open " + filepath.string());
	std::string line;
	std::smatch match;
	if (!std::getline(input, line) || !std::regex_match(line, match, rx_ucd_version))
		throw makeunicode_error("unable to read version info from " + filepath.string());
	version = match.str(1);
	return input;
}

template <class StoreFunc>
std::string read_ucd_array(stdfs::path filepath, StoreFunc store)
{
	std::string version;
	std::ifstream input = open_ucd_file_and_read_version(filepath, version);
	try {
		std::string line;
		while (std::getline(input, line)) {
			std::smatch match;
			if (!line.empty() && line[0] != '#' && std::regex_match(line, match, rx_ucd_prop_range)) {
				auto start = std::stoul(match.str(1), nullptr, 16);
				auto end = match.length(2) > 0 ? std::stoul(match.str(3), nullptr, 16) : start;
				store(start, end, match.str(4));
			}
		}
	} catch (std::exception& e) {
		throw makeunicode_error("unable to read data from " + filepath.string() + "\n -- " + e.what());
	}
	return version;
}

// Filters for deriving POSIX compatibility table from binary properties and general
// categories, based on Unicode TR#18, Annex C: Compatibility Properties.

namespace compat_filter_arg
{
	constexpr std::size_t cflags = 0;
	constexpr std::size_t pflags = 1;
	constexpr std::size_t gcindex = 2;
};

using compat_filter_args = std::tuple<std::uint_least16_t, std::uint_least64_t, std::uint_least8_t>;
using compat_filter = std::function<bool(compat_filter_args const&)>;

template <std::size_t ArgIndex, class NameMap>
compat_filter make_bitfield_compat_filter(NameMap const& namemap, std::initializer_list<std::string> names)
{
	using flag_type = typename NameMap::mapped_type;

	auto flags = std::accumulate(
		std::begin(names),
		std::end(names),
		flag_type{0},
		[&namemap](auto x, auto n) { return static_cast<flag_type>(x | namemap.find(n)->second); });

	return [flags](compat_filter_args const& args) { return (std::get<ArgIndex>(args) & flags) != 0; };
}

compat_filter make_compat_filter(std::initializer_list<std::string> names)
{
	return make_bitfield_compat_filter<compat_filter_arg::cflags>(compatibility_properties, std::move(names));
}

compat_filter make_binary_prop_compat_filter(std::initializer_list<std::string> names)
{
	return make_bitfield_compat_filter<compat_filter_arg::pflags>(binary_properties, std::move(names));
}

compat_filter make_general_category_compat_filter(std::initializer_list<std::string> names)
{
	std::vector<std::uint_least8_t> indices;
	indices.reserve(std::size(names));

	std::transform(
		std::begin(names),
		std::end(names),
		std::back_inserter(indices),
		[](auto name) { return general_categories.find(name)->second; });

	return [indices = std::move(indices)](compat_filter_args const& args) {
		return std::any_of(
			std::begin(indices),
			std::end(indices),
			[gcindex = std::get<compat_filter_arg::gcindex>(args)](auto index) { return index == gcindex; });
	};
}

// Reads in the UCD files and generates all of the uncompressed property tables.

void read_and_build_tables()
{
	auto pending_versions = std::async(std::launch::async, [] {
		// read in binary property table
		auto pending_prop_versions = std::async(std::launch::async, [] {
			auto set_ptable_bits = [](auto start, auto end, auto const& value) {
				if (auto prop = binary_properties.find(value); prop != binary_properties.end())
					std::transform(ptable.begin() + start, ptable.begin() + end + 1, ptable.begin() + start,
							[bitflag = prop->second](auto const& old) { return bitflag | old; });
			};
			std::fill(std::execution::par_unseq, ptable.begin(), ptable.end(), binary_properties.find("Any")->second);
			set_ptable_bits(0, 128, "Ascii");
			auto proplist_version = read_ucd_array("PropList.txt", set_ptable_bits);
			auto dcp_version = read_ucd_array("DerivedCoreProperties.txt", set_ptable_bits);
			return std::vector<std::string>{proplist_version, dcp_version};
		});

		// read in general category table
		auto pending_gcversion = std::async(std::launch::async, [] {
			std::fill(std::execution::par_unseq, gctable.begin(), gctable.end(), general_categories.find("Cn")->second);
			return read_ucd_array("DerivedGeneralCategory.txt", [](auto start, auto end, auto const& value) {
				if (auto category = general_categories.find(value); category != general_categories.end())
					std::fill(std::execution::par_unseq, gctable.begin() + start, gctable.begin() + end + 1, category->second);
			});
		});

		// setup compatibility filter maps while reading tables, see Unicode TR#18,
		// Annex C: Compatibility Properties
		using compat_filter_map = std::unordered_map<std::string, std::vector<compat_filter>>;

		compat_filter_map compat_prop_includes = {
			{ "alpha", { make_binary_prop_compat_filter({"Alphabetic"}) } },
			{ "lower", { make_binary_prop_compat_filter({"Lowercase"}) } },
			{ "upper", { make_binary_prop_compat_filter({"Uppercase"}) } },
			{ "punct", { make_general_category_compat_filter({"Pc", "Pd", "Pe", "Pf", "Pi", "Po", "Ps"}) } },
			{ "digit", { make_general_category_compat_filter({"Nd"}) } },
			{ "xdigit", { make_compat_filter({"digit"}), make_binary_prop_compat_filter({"Hex_Digit"}) } },
			{ "alnum", { make_compat_filter({"alpha"}), make_compat_filter({"digit"}) } },
			{ "space", { make_binary_prop_compat_filter({"White_Space"}) } },
			{ "blank", { make_general_category_compat_filter({"Zs"}) } },
			{ "cntrl", { make_general_category_compat_filter({"Cc"}) } },
			{ "graph", { make_binary_prop_compat_filter({"Assigned"}) } },
			{ "print", { make_compat_filter({"blank", "graph"}) } },
			{ "word", { make_compat_filter({"alnum"}), make_binary_prop_compat_filter({"Join_Control"}),
				make_general_category_compat_filter({"Mn", "Mc", "Me", "Pc"}) } }
		};

		compat_filter_map compat_prop_excludes = {
			{ "graph", { make_compat_filter({"space"}), make_general_category_compat_filter({"Cc", "Cs"}) } },
			{ "print", { make_compat_filter({"cntrl"}) } }
		};

		// make sure reading of ptable and gctable is complete, as they're
		// needed to build the compatibility property table
		auto prop_versions = pending_prop_versions.get();
		prop_versions.push_back(pending_gcversion.get());

		// for all non-unassigned codepoints on gctable, set Assigned flag in ptable
		auto const gcunassignedindex = general_categories.find("Cn")->second;
		auto const passignedflag = binary_properties.find("Assigned")->second;
		for (std::size_t i = 0; i < gctable.size(); ++i)
			if (gctable[i] != gcunassignedindex)
				ptable[i] |= passignedflag;

		// manually set extra properties for tab character
		ctable[0x09] = compatibility_properties.find("blank")->second | compatibility_properties.find("print")->second;

		// derive compatibility table by filtering through ptable and gctable
		std::for_each(
			std::execution::par_unseq,
			make_integer_iterator<std::size_t>(0),
			make_integer_iterator(ctable.size()),
			[&](auto i) {
				auto args = std::make_tuple(ctable[i], ptable[i], gctable[i]);
				for (std::size_t j = 0, n = compatibility_property_names.size(); j < n; ++j) {
					auto cname = compatibility_property_names[j];
					auto cflag = std::uint_least16_t{1} << j;
					for (auto const& include : compat_prop_includes[cname])
						if (include(args))
							std::get<compat_filter_arg::cflags>(args) |= cflag;
					if (compat_prop_excludes.count(cname) != 0)
						for (auto const& exclude : compat_prop_excludes[cname])
							if (exclude(args))
								std::get<compat_filter_arg::cflags>(args) &= ~cflag;
				}
				ctable[i] = std::get<compat_filter_arg::cflags>(args);
			});

		return prop_versions;
	});

	// load in the scripts table
	auto pending_scversion = std::async(std::launch::async, [] {
		std::fill(std::execution::par_unseq, sctable.begin(), sctable.end(), scripts.find("Unknown")->second);
		return read_ucd_array("Scripts.txt", [](auto start, auto end, auto const& value) {
			if (auto script = scripts.find(value); script != scripts.end())
				std::fill(std::execution::par_unseq, sctable.begin() + start, sctable.begin() + end + 1, script->second);
		});
	});

	// make sure versions from all ucd files match one another
	auto versions = pending_versions.get();
	versions.push_back(pending_scversion.get());
	if (std::any_of(versions.begin(), versions.end(), [&versions](auto const& v) { return v != versions.front(); }))
		throw makeunicode_error("unicode version mismatch between input files");
}

struct ucd_type_info
{
	std::string_view name;
	std::string_view cmacro;
	unsigned int szbytes = 0;
	unsigned int digits10 = 0;
};

#define make_unsigned_types_entry(T, C) { (std::numeric_limits<T>::max)(), { #T, C, static_cast<unsigned int>(sizeof(T)) } }

std::map<std::uintmax_t, std::tuple<std::string_view, std::string_view, unsigned int>> const unsigned_integer_types =
{
	make_unsigned_types_entry(std::uint_least8_t, "UINT8_C"), make_unsigned_types_entry(std::uint_least16_t, "UINT16_C"),
	make_unsigned_types_entry(std::uint_least32_t, "UINT32_C"), make_unsigned_types_entry(std::uint_least64_t, "UINT64_C")
};

#undef make_unsigned_types_entry

auto get_maxval_type_info(std::uintmax_t maxval)
{
	auto[type, cmacro, size] = unsigned_integer_types.upper_bound((std::min)(maxval, maxval - 1))->second;
	return ucd_type_info{type, cmacro, size, static_cast<unsigned int>(std::to_string(maxval).size())};
}

inline auto get_table_type_info(std::vector<std::size_t> const& table)
{
	return get_maxval_type_info(*std::max_element(table.cbegin(), table.cend()));
}

template <class T>
inline auto get_type_info()
{
	return get_maxval_type_info((std::numeric_limits<T>::max)());
}

struct ucd_record
{
	std::uint_least64_t pflags;
	std::uint_least16_t cflags;
	std::uint_least8_t gcindex;
	std::uint_least8_t scindex;
};

constexpr bool operator==(ucd_record const& x, ucd_record const& y) noexcept
{
	return x.pflags == y.pflags && x.cflags == y.cflags && x.gcindex == y.gcindex && x.scindex == y.scindex;
}

constexpr bool operator!=(ucd_record const& x, ucd_record const& y) noexcept
{
	return !(x == y);
}

struct ucd_record_hash
{
	std::size_t operator()(ucd_record const& record) const noexcept {
		return hash_combine(
			std::hash<std::uint_least64_t>{}(record.pflags), std::hash<std::uint_least16_t>{}(record.cflags),
			std::hash<std::uint_least8_t>{}(record.gcindex), std::hash<std::uint_least8_t>{}(record.scindex));
	}
};

struct ucd_record_stage_table
{
	std::vector<std::size_t> stage1;
	std::vector<std::size_t> stage2;
	ucd_type_info typeinfo1;
	ucd_type_info typeinfo2;
	std::size_t block_size = 0;
	std::size_t table_size = 0;
};

static std::size_t invalidrecordindex;
static std::unordered_map<ucd_record, std::size_t, ucd_record_hash> recordindices;
static std::vector<ucd_record> recordvalues;
static ucd_array<std::size_t> recordtable;
static ucd_record_stage_table recordstagetable;

std::size_t combine_record(ucd_record const& record)
{
	auto emplacement = recordindices.try_emplace(record, recordvalues.size());
	if (emplacement.second)
		recordvalues.push_back(record);
	return emplacement.first->second;
}

void combine_records()
{
	for (std::size_t i = 0, n = ptable.size(); i < n; ++i)
		recordtable[i] = combine_record({ptable[i], ctable[i], gctable[i], sctable[i]});
	ucd_record invalidrecord{binary_properties.find("Any")->second, 0, general_categories.find("Cn")->second, scripts.find("Unknown")->second};
	invalidrecordindex = combine_record(invalidrecord);
}

void compress_records()
{
	static constexpr unsigned int min_log2_block_size = 4, max_log2_block_size = 12;
	static constexpr unsigned int trial_count = max_log2_block_size - min_log2_block_size;

	std::array<ucd_record_stage_table, trial_count> trials;

	std::for_each_n(std::execution::par_unseq, make_integer_iterator(min_log2_block_size), trial_count, [&trials](auto b) {
		std::size_t stagedblockcount = 0;
		std::unordered_multimap<std::size_t, std::size_t> stagedblockmap;

		auto& trial = trials[b - min_log2_block_size];
		trial.block_size = std::size_t{1} << b;
		assert(recordtable.size() % trial.block_size == 0);

		auto blockit = recordtable.cbegin(), endblockit = recordtable.cend();
		while (blockit < endblockit) {
			auto blockfirst = blockit, blocklast = std::next(blockit, trial.block_size);
			auto blockhash = std::accumulate(blockfirst, blocklast, std::size_t{0}, [](std::size_t x, std::size_t y) {
				return hash_combine(std::hash<std::size_t>{}(y), x);
			});
			auto [match, lastmatch] = stagedblockmap.equal_range(blockhash);
			for ( ; match != lastmatch; ++match) {
				if (std::equal(blockfirst, blocklast, std::next(trial.stage2.cbegin(), match->second * trial.block_size)))
					break;
			}
			if (match == lastmatch) {
				trial.stage2.insert(trial.stage2.end(), blockfirst, blocklast);
				match = stagedblockmap.emplace(blockhash, stagedblockcount++);
			}
			trial.stage1.push_back(match->second);
			blockit = blocklast;
		}

		trial.typeinfo1 = get_table_type_info(trial.stage1);
		trial.typeinfo2 = get_table_type_info(trial.stage2);
		trial.table_size = trial.typeinfo1.szbytes * trial.stage1.size() + trial.typeinfo2.szbytes * trial.stage2.size();
	});

	auto smallest = std::min_element(trials.begin(), trials.end(), [](auto& x, auto& y) {
		return x.table_size < y.table_size;
	});

	recordstagetable = std::move(*smallest);
}

auto run_length_encode(std::vector<std::size_t> const& input, ucd_type_info const& info)
{
	auto const maxseqlen = (std::size_t{1} << ((CHAR_BIT * info.szbytes) - 2)) - 1;
	auto const seqmask = std::size_t{3} << ((CHAR_BIT * info.szbytes) - 2);

	std::vector<std::size_t> output;

	for (auto first = input.begin(), last = input.end(); first != last; ) {
		auto value = *first;
		auto next = std::find_if_not(std::next(first), last, [value](auto x) { return x == value; });
		auto count = static_cast<std::size_t>(std::distance(first, next));
		if (count > 1 || (value & seqmask) == seqmask) {
			do {
				auto seqlen = (std::min)(count - 1, maxseqlen);
				output.push_back(seqmask | static_cast<std::size_t>(seqlen));
				output.push_back(value);
				count -= seqlen + 1;
			} while (count > maxseqlen);
		} else {
			output.push_back(value);
		}
		first = next;
	}

	return output;
}

template <class InputIt>
inline auto max_element_size(InputIt first, InputIt last)
{
	return std::max_element(first, last, [](auto& x, auto& y) { return x.size() < y.size(); })->size();
}

template <class Integral, class = std::enable_if_t<std::is_integral_v<Integral>>>
inline int align_padding(Integral value) noexcept
{
	return ((static_cast<int>(value) + 3) / 4) * 4;
}

template <class IndexMap, class ValueSequence, class Value>
auto intern_value(IndexMap& indices, ValueSequence& values, Value value)
{
	using index_type = typename IndexMap::mapped_type;
	auto emplacement = indices.try_emplace(value, static_cast<index_type>(values.size()));
	if (emplacement.second) {
		assert(values.size() < (std::numeric_limits<index_type>::max)());
		values.push_back(value);
	}
	return emplacement.first;
}

auto normalize_property_label(std::string_view id)
{
	std::string normid;
	for (char c : id)
		if (c != ' ' && c != '\t' && c != '_' && c != '-')
			normid.push_back(static_cast<char>(std::tolower(c)));
	return normid;
}

template <class ValueSequence>
std::ostream& print_table(std::ostream& out, std::string_view name, std::string_view type, std::string_view cmacro,
		ValueSequence const& values, std::size_t columnlen, const char* indent, const char* endline)
{
	out << indent << "static constexpr std::array<" << type << ", " << std::dec << values.size() << "> " << name << " =\n";
	out << indent << "{" << endline;
	std::size_t linelen = 0;
	for (auto first = values.begin(), last = values.end(); first != last; ++first) {
		auto segment = std::to_string(*first);
		if (!cmacro.empty())
			segment = std::string{cmacro} + "(" + segment + ")";
		out << segment;
		if (first != last - 1)
			out << ',' << ((linelen += segment.size() + 2) > columnlen ? linelen = 0, endline : " ");
	}
	return out << "\n" << indent << "};\n";
}

class enum_printer
{
	enum_type enum_type_;
	std::string_view name_, type_, comment_;
	std::function<void(std::ostream&)> body_;

public:
	template <class BodyPrinter>
	enum_printer(enum_type etype, std::string_view name, std::string_view type, std::string_view comment, BodyPrinter body)
		: enum_type_{etype}, name_{name}, type_{type}, comment_{comment}, body_{std::move(body)} {}

	friend std::ostream& operator<<(std::ostream& out, enum_printer const& p) {
		out << "// " << p.comment_ << "\n";
		out << "enum class " << p.name_ << " : " << p.type_ << "\n{\n";
		if (p.enum_type_ == enum_type::bitfield)
			out << "\tis_bitfield_enum,\n";
		p.body_(out);
		return out << "};\n";
	}
};

class enum_parser_printer
{
	std::string_view name_, abbr_;
	std::function<std::vector<std::pair<std::string, std::string>>()> label_source_;

public:
	template <class LabelSource>
	enum_parser_printer(std::string_view name, std::string_view abbr, LabelSource label_source)
		: name_{name}, abbr_{abbr}, label_source_{std::move(label_source)} {}

	friend std::ostream& operator<<(std::ostream& out, enum_parser_printer const& p)
	{
		auto labels = p.label_source_();
		std::sort(labels.begin(), labels.end(), [](auto const& x, auto const& y) { return x.first < y.first; });

		out << "// Convert from text to " << p.name_ << " property\n"
			<< "inline std::optional<" << p.name_ << "> sto" << p.name_ << "(std::string_view s)\n"
			<< "{\n"
			<< "\tusing namespace std::string_view_literals;\n"
			<< "\tusing " << p.abbr_ << " = " << p.name_ << ";\n\n"
			<< "\tstatic constexpr std::array<std::pair<std::string_view, " << p.name_ << ">, " << std::dec << labels.size() << "> labels =\n"
			<< "\t{ {\n";

		std::string line, segment;
		line.reserve(150);
		segment.reserve(80);

		for (std::size_t i = 0, n = labels.size(); i < n; ++i) {
			segment = "{ \"" + labels[i].first + "\"sv, ";
			segment.append(p.abbr_.data(), p.abbr_.size());
			segment += "::" + labels[i].second + " }";
			if (i < n - 1) {
				segment += ',';
				if (segment.size() + line.size() > 120) {
					out << "\t\t" << line << "\n";
					line = segment;
					continue;
				}
			}
			if (i > 0)
				line += ' ';
			line += segment;
		}

		return out << "\t\t" << line << "\n"
			<< "\t} };\n\n"
			<< "\tauto l = detail::normalize_property_label(s);\n"
			<< "\tauto c = std::lower_bound(labels.begin(), labels.end(), l, [](auto const& x, auto const& y) { return x.first < y; });\n"
			<< "\treturn c != labels.end() && c->first == l ? std::optional<" << p.name_ << ">{static_cast<" << p.name_ << ">(c->second)} : std::nullopt;\n"
			<< "}\n";
	}
};

class rle_stage_table_printer
{
	std::string_view name_;
	std::vector<std::size_t> const& table_;
	ucd_type_info const& typeinfo_;

public:
	rle_stage_table_printer(std::string_view name, std::vector<std::size_t> const& table, ucd_type_info const& typeinfo)
		: name_{name}, table_{table}, typeinfo_{typeinfo} {}

	friend std::ostream& operator<<(std::ostream& out, rle_stage_table_printer const& p) {
		return print_table(out, p.name_, p.typeinfo_.name, "", run_length_encode(p.table_, p.typeinfo_), 120, "\t", "\n\t\t");
	}
};

class record_flyweight_printer
{
	std::vector<ucd_record> const& records_;

public:
	explicit record_flyweight_printer(std::vector<ucd_record> const& records)
		: records_{records} {}

	friend std::ostream& operator<<(std::ostream& out, record_flyweight_printer const& p) {
		std::unordered_map<std::uint_least64_t, std::uint_least8_t> pflag_indices;
		std::unordered_map<std::uint_least16_t, std::uint_least8_t> cflag_indices;
		std::vector<std::uint_least64_t> pflag_values;
		std::vector<std::uint_least16_t> cflag_values;
		std::vector<std::uint_least8_t> flyweights;

		for (auto const& record : p.records_) {
			flyweights.push_back(intern_value(pflag_indices, pflag_values, record.pflags)->second);
			flyweights.push_back(intern_value(cflag_indices, cflag_values, record.cflags)->second);
			flyweights.push_back(record.gcindex);
			flyweights.push_back(record.scindex);
		}

		print_table(out, "flyweights", "std::uint_least8_t", "", flyweights, 120, "\t", "\n\t\t") << "\n";
		print_table(out, "pflags", "std::uint_least64_t", "UINT64_C", pflag_values, 120, "\t", "\n\t\t") << "\n";
		print_table(out, "cflags", "std::uint_least16_t", "", cflag_values, 120, "\t", "\n\t\t");
		return out;
	}
};

void print_unicode_header()
{
double log2_block_size = std::ceil(std::log2(static_cast<double>(recordstagetable.block_size)));
std::size_t const block_shift = static_cast<std::size_t>(std::lrint(log2_block_size));
std::size_t const block_mask = (1u << block_shift) - 1;

std::cout <<
R"c++(// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017 Jesse W. Towner
// See LICENSE.md file for license details

// This header file is generated by the makeunicode tool program.
// Do not modify this file by hand. Instead, modify and run the
// tool to regenerate this file.

#ifndef LUG_UNICODE_HPP__
#define LUG_UNICODE_HPP__

#include <lug/detail.hpp>
#include <cctype>
#include <cstddef>
#include <cstdint>
#include <array>
#include <memory>
#include <optional>
#include <utility>

namespace lug::unicode
{
)c++"
<< "\n"
<< enum_printer(enum_type::bitfield, "ctype", "std::uint_least16_t", "POSIX compatibility properties", [](std::ostream& out) {
	auto const& cnames = compatibility_property_names;
	auto const cpad = align_padding(max_element_size(cnames.cbegin(), cnames.cend()));
	for (std::size_t i = 0, n = cnames.size(); i < n; ++i)
		out << "\t" << std::left << std::setw(cpad) << cnames[i] << " = UINT16_C(1) << " << std::right << std::setw(2) << i << ",\n";
	out << "\t" << std::left << std::setw(cpad) << "none" << " = 0\n";
})
<< "\n"
<< enum_printer(enum_type::bitfield, "ptype", "std::uint_least64_t", "Unicode binary properties", [](std::ostream& out) {
	auto const& pnames = binary_property_names;
	auto const ppad = align_padding(max_element_size(pnames.cbegin(), pnames.cend()));
	for (std::size_t i = 0, n = pnames.size(); i < n; ++i)
		out << "\t" << std::left << std::setw(ppad) << pnames[i] << " = UINT64_C(1) << " << std::right << std::setw(2) << i << ",\n";
	out << "\t" << std::left << std::setw(ppad) << "None" << " = 0\n";
})
<< "\n"
<< enum_printer(enum_type::bitfield, "gctype", "std::uint_least32_t", "Unicode general categories", [](std::ostream& out) {
	auto const& gcnames = general_category_names;
	auto const& gclnames = general_category_long_names;
	auto const gclpad = align_padding(max_element_size(gclnames.cbegin(), gclnames.cend()));
	for (std::size_t i = 0, n = gcnames.size(); i < n; ++i)
		out << "\t" << gcnames[i] << " = UINT32_C(1) << " << std::right << std::setw(2) << i << ",    " << gclnames[i] << " = " << gcnames[i] << ",\n";
	for (auto const& compound : compound_general_categories) {
		out << "\t" << std::left << std::setw(2) << compound.first << " = ";
		auto const& components = compound.second.second;
		int padcount = 0;
		for (std::size_t i = 0, n = components.size(); i < n; ++i) {
			out << components[i];
			padcount += static_cast<int>(components[i].size());
			if (i < n - 1) {
				out << "|";
				++padcount;
			}
		}
		out << "," << std::right << std::setw(21 - padcount) << " " << compound.second.first << " = " << compound.first << ",\n";
	}
	out << "\t" << "None = 0\n";
})
<< "\n"
<< enum_printer(enum_type::index, "sctype", "std::uint_least8_t", "Unicode scripts", [](std::ostream& out) {
	auto const& scnames = script_names;
	auto const scpad = align_padding(max_element_size(scnames.cbegin(), scnames.cend()));
	for (std::size_t i = 0, n = scnames.size(); i < n; ++i)
		out << "\t" << std::left << std::setw(scpad) << scnames[i] << " = " << std::right << std::setw(3) << i << (i < n - 1 ? ",\n" : "\n");
})
<< R"c++(
// Unicode Character Database (UCD) record
class ucd_record
{
	struct raw_record {
		std::uint_least64_t pflags;
		std::uint_least16_t cflags;
		std::uint_least8_t gcindex;
		std::uint_least8_t scindex;
	} const* record_;
	explicit ucd_record(raw_record const* r) noexcept : record_(r) {}
	struct raw_record_table {)c++"
		<< "\n\t\tstd::array<" << recordstagetable.typeinfo1.name << ", " << std::dec << recordstagetable.stage1.size() << "> stage1;\n"
		<< "\t\tstd::array<" << recordstagetable.typeinfo2.name << ", " << std::dec << recordstagetable.stage2.size() << "> stage2;\n"
		<< "\t\tstd::array<raw_record, " << std::dec << recordvalues.size() << "> records;" << R"c++(
	};
	static std::unique_ptr<raw_record_table> decompress_table();
	friend ucd_record query(char32_t r);
public:
	ctype compatibility() const noexcept { return static_cast<ctype>(record_->cflags); }
	ptype properties() const noexcept { return static_cast<ptype>(record_->pflags); }
	gctype general_category() const noexcept { return static_cast<gctype>(UINT32_C(1) << record_->gcindex); }
	sctype script() const noexcept { return static_cast<sctype>(record_->scindex); }
	bool any_of(ctype c) const noexcept { return (compatibility() & c) != ctype::none; }
	bool any_of(ptype p) const noexcept { return (properties() & p) != ptype::None; }
	bool any_of(gctype gc) const noexcept { return (general_category() & gc) != gctype::None; }
};

// Retrieves the UCD record for the given Unicode codepoint
inline ucd_record query(char32_t r)
{
	static auto const table = ucd_record::decompress_table();
	std::size_t index = )c++" << std::dec << invalidrecordindex << R"c++(;
	if (r < 0x)c++" << std::hex << ptable.size() << R"c++() {
		index = table->stage1[r >> )c++" << std::dec << block_shift << R"c++(];
		index = table->stage2[(index << )c++" << std::dec << block_shift << R"c++() | (r & 0x)c++" << std::hex << block_mask << R"c++()];
	}
	return ucd_record{table->records.data() + index};
}

namespace detail
{

inline std::string normalize_property_label(std::string_view id)
{
	std::string normid;
	for (char c : id)
		if (c != ' ' && c != '\t' && c != '_' && c != '-')
			normid.push_back(static_cast<char>(std::tolower(c)));
	return normid;
}

} // namespace detail

)c++" << enum_parser_printer("ctype", "ct", [] {
	std::vector<std::pair<std::string, std::string>> labels;
	std::transform(compatibility_property_names.begin(), compatibility_property_names.end(), std::back_inserter(labels),
			[](auto& label) { return std::make_pair(normalize_property_label(label), label); });
	return labels;
})
<< "\n"
<< enum_parser_printer("ptype", "pt", [] {
	std::vector<std::pair<std::string, std::string>> labels;
	std::transform(binary_property_names.begin(), binary_property_names.end(), std::back_inserter(labels),
			[](auto& label) { return std::make_pair(normalize_property_label(label), label); });
	return labels;
})
<< "\n"
<< enum_parser_printer("gctype", "gc", [] {
	std::vector<std::pair<std::string, std::string>> labels;
	for (std::size_t i = 0, n = general_category_names.size(); i < n; ++i) {
		labels.emplace_back(normalize_property_label(general_category_names[i]), general_category_names[i]);
		labels.emplace_back(normalize_property_label(general_category_long_names[i]), general_category_names[i]);
	}
	return labels;
})
<< "\n"
<< enum_parser_printer("sctype", "sc", [] {
	std::vector<std::pair<std::string, std::string>> labels;
	std::transform(script_names.begin(), script_names.end(), std::back_inserter(labels),
			[](auto& label) { return std::make_pair(normalize_property_label(label), label); });
	return labels;
})
<< R"c++(
namespace detail
{

template <class InputIt, class OutputIt>
void run_length_decode(InputIt first, InputIt last, OutputIt dest)
{
	using value_type = typename std::iterator_traits<InputIt>::value_type;
	constexpr auto seqmask = static_cast<value_type>(0x03ull << (std::numeric_limits<value_type>::digits - 2));
	while (first != last) {
		auto const lead = *first++;
		if ((lead & seqmask) == seqmask)
			dest = ::std::fill_n(dest, static_cast<std::size_t>(lead & ~seqmask) + 1, *first++);
		else
			*dest++ = lead;
	}
}

} // namespace detail

inline std::unique_ptr<ucd_record::raw_record_table> ucd_record::decompress_table()
{
)c++"
<< rle_stage_table_printer("rlestage1", recordstagetable.stage1, recordstagetable.typeinfo1) << "\n"

<< rle_stage_table_printer("rlestage2", recordstagetable.stage2, recordstagetable.typeinfo2) << "\n"

<< record_flyweight_printer(recordvalues)
<< R"c++(
	auto table = std::make_unique<raw_record_table>();
	detail::run_length_decode(rlestage1.begin(), rlestage1.end(), table->stage1.begin());
	detail::run_length_decode(rlestage2.begin(), rlestage2.end(), table->stage2.begin());
	auto& records = table->records;
	for (std::size_t r = 0, f = 0, e = flyweights.size(); f < e; ++r, f += 4) {
		records[r].pflags = pflags[flyweights[f + 0]];
		records[r].cflags = cflags[flyweights[f + 1]];
		records[r].gcindex = flyweights[f + 2];
		records[r].scindex = flyweights[f + 3];
	}
	return table;
}

} // namespace lug::unicode

#endif
)c++";
}

int main(int, char**)
{
	try {
		read_and_build_tables();
		combine_records();
		compress_records();
		print_unicode_header();
	} catch (std::exception& e) {
		std::cerr << "Error: " << e.what() << "\nExiting...\n" << std::flush;
		return 1;
	} catch (...) {
		std::cerr << "Unknown error occurred!\nExiting...\n" << std::flush;
		return 1;
	}
	return 0;
}
