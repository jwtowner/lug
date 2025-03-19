// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#include <cassert>
#include <cctype>
#include <climits>
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
#include <numeric>
#include <regex>
#include <set>
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
	using value_type = Integer;
	using reference = value_type const&;
	using pointer = value_type const*;
	using difference_type = std::common_type_t<std::ptrdiff_t, std::make_signed_t<Integer>>;
	using iterator_category = std::random_access_iterator_tag;
	constexpr integer_iterator() noexcept : value_{0} {}
	constexpr explicit integer_iterator(Integer value) : value_{value} {}
	reference operator*() const { return value_; }
	value_type operator[](difference_type n) const { return static_cast<value_type>(static_cast<difference_type>(value_) + n); }
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
	friend constexpr integer_iterator operator+(integer_iterator const& x, difference_type n) noexcept { return integer_iterator{static_cast<value_type>(static_cast<difference_type>(x.value_) + n)}; }
	friend constexpr integer_iterator operator+(difference_type n, integer_iterator const& x) noexcept { return integer_iterator{static_cast<value_type>(static_cast<difference_type>(x.value_) + n)}; }
	friend constexpr integer_iterator operator-(integer_iterator const& x, difference_type n) noexcept { return integer_iterator{static_cast<value_type>(static_cast<difference_type>(x.value_) - n)}; }
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
	// Custom
	"Any", "Ascii", "Assigned", "Line_Ending",
	// PropList.txt
	"White_Space", "Bidi_Control", "Join_Control", "Dash", "Quotation_Mark", "Terminal_Punctuation",
	"Other_Math", "Hex_Digit", "ASCII_Hex_Digit", "Other_Alphabetic", "Ideographic", "Diacritic", "Extender",
	"Other_Lowercase", "Other_Uppercase", "Noncharacter_Code_Point", "Other_Grapheme_Extend",
	"IDS_Binary_Operator", "IDS_Trinary_Operator", "IDS_Unary_Operator", "Radical", "Unified_Ideograph",
	"Other_Default_Ignorable_Code_Point", "Deprecated", "Soft_Dotted", "Logical_Order_Exception",
	"Other_ID_Start", "Other_ID_Continue", "ID_Compat_Math_Continue", "ID_Compat_Math_Start",
	"Sentence_Terminal", "Variation_Selector", "Pattern_White_Space", "Pattern_Syntax",
	"Prepended_Concatenation_Mark", "Regional_Indicator", "Modifier_Combining_Mark",
	// DerivedCoreProperties.txt
	"Lowercase", "Uppercase", "Cased", "Case_Ignorable", "Changes_When_Lowercased", "Changes_When_Uppercased",
	"Changes_When_Titlecased", "Changes_When_Casefolded", "Changes_When_Casemapped", "Alphabetic",
	"Default_Ignorable_Code_Point", "Grapheme_Base", "Grapheme_Extend", "Math", "ID_Start", "ID_Continue",
	"XID_Start", "XID_Continue", "Grapheme_Link", "InCB; Linker", "InCB; Consonant", "InCB; Extend",
	// DerivedBinaryProperties.txt
	"Bidi_Mirrored"
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
	"Adlam", "Bhaiksuki", "Marchen", "Newa", "Osage", "Tangut", "Masaram_Gondi", "Nushu", "Soyombo", "Zanabazar_Square",
	// Unicode 11.0.0
	"Dogra", "Gunjala_Gondi", "Makasar", "Medefaidrin", "Hanifi_Rohingya", "Sogdian", "Old_Sogdian",
	// Unicode 12.0.0
	"Elymaic", "Nandinagari", "Nyiakeng_Puachue_Hmong", "Wancho",
	// Unicode 13.0.0
	"Chorasmian", "Dives_Akuru", "Khitan_Small_Script", "Yezidi",
	// Unicode 14.0.0
	"Cypro_Minoan", "Old_Uyghur", "Tangsa", "Toto", "Vithkuqi",
	// Unicode 15.0.0
	"Kawi", "Nag_Mundari",
	// Unicode 16.0.0
	"Garay", "Gurung_Khema", "Kirat_Rai", "Ol_Onal", "Sunuwar", "Todhri", "Tulu_Tigalari"
};

static auto const scripts = build_namemap<enum_type::index, std::uint_least8_t>(script_names);

static std::vector<std::string> const block_names =
{
	"No_block", "Basic Latin", "Latin-1 Supplement", "Latin Extended-A", "Latin Extended-B", "IPA Extensions",
	"Spacing Modifier Letters", "Combining Diacritical Marks", "Greek and Coptic", "Cyrillic", "Cyrillic Supplement",
	"Armenian", "Hebrew", "Arabic", "Syriac", "Arabic Supplement", "Thaana", "NKo", "Samaritan", "Mandaic", "Syriac Supplement",
	"Arabic Extended-A", "Devanagari", "Bengali", "Gurmukhi", "Gujarati", "Oriya", "Tamil", "Telugu", "Kannada", "Malayalam",
	"Sinhala", "Thai", "Lao", "Tibetan", "Myanmar", "Georgian", "Hangul Jamo", "Ethiopic", "Ethiopic Supplement", "Cherokee",
	"Unified Canadian Aboriginal Syllabics", "Ogham", "Runic", "Tagalog", "Hanunoo", "Buhid", "Tagbanwa", "Khmer", "Mongolian",
	"Unified Canadian Aboriginal Syllabics Extended", "Limbu", "Tai Le", "New Tai Lue", "Khmer Symbols", "Buginese", "Tai Tham",
	"Combining Diacritical Marks Extended", "Balinese", "Sundanese", "Batak", "Lepcha", "Ol Chiki", "Cyrillic Extended-C",
	"Sundanese Supplement", "Vedic Extensions", "Phonetic Extensions", "Phonetic Extensions Supplement",
	"Combining Diacritical Marks Supplement", "Latin Extended Additional", "Greek Extended", "General Punctuation",
	"Superscripts and Subscripts", "Currency Symbols", "Combining Diacritical Marks for Symbols", "Letterlike Symbols",
	"Number Forms", "Arrows", "Mathematical Operators", "Miscellaneous Technical", "Control Pictures",
	"Optical Character Recognition", "Enclosed Alphanumerics", "Box Drawing", "Block Elements", "Geometric Shapes",
	"Miscellaneous Symbols", "Dingbats", "Miscellaneous Mathematical Symbols-A", "Supplemental Arrows-A", "Braille Patterns",
	"Supplemental Arrows-B", "Miscellaneous Mathematical Symbols-B", "Supplemental Mathematical Operators",
	"Miscellaneous Symbols and Arrows", "Glagolitic", "Latin Extended-C", "Coptic", "Georgian Supplement", "Tifinagh",
	"Ethiopic Extended", "Cyrillic Extended-A", "Supplemental Punctuation", "CJK Radicals Supplement", "Kangxi Radicals",
	"Ideographic Description Characters", "CJK Symbols and Punctuation", "Hiragana", "Katakana", "Bopomofo",
	"Hangul Compatibility Jamo", "Kanbun", "Bopomofo Extended", "CJK Strokes", "Katakana Phonetic Extensions",
	"Enclosed CJK Letters and Months", "CJK Compatibility", "CJK Unified Ideographs Extension A", "Yijing Hexagram Symbols",
	"CJK Unified Ideographs", "Yi Syllables", "Yi Radicals", "Lisu", "Vai", "Cyrillic Extended-B", "Bamum",
	"Modifier Tone Letters", "Latin Extended-D", "Syloti Nagri", "Common Indic Number Forms", "Phags-pa", "Saurashtra",
	"Devanagari Extended", "Kayah Li", "Rejang", "Hangul Jamo Extended-A", "Javanese", "Myanmar Extended-B", "Cham",
	"Myanmar Extended-A", "Tai Viet", "Meetei Mayek Extensions", "Ethiopic Extended-A", "Latin Extended-E",
	"Cherokee Supplement", "Meetei Mayek", "Hangul Syllables", "Hangul Jamo Extended-B", "High Surrogates",
	"High Private Use Surrogates", "Low Surrogates", "Private Use Area", "CJK Compatibility Ideographs",
	"Alphabetic Presentation Forms", "Arabic Presentation Forms-A", "Variation Selectors", "Vertical Forms",
	"Combining Half Marks", "CJK Compatibility Forms", "Small Form Variants", "Arabic Presentation Forms-B",
	"Halfwidth and Fullwidth Forms", "Specials", "Linear B Syllabary", "Linear B Ideograms", "Aegean Numbers",
	"Ancient Greek Numbers", "Ancient Symbols", "Phaistos Disc", "Lycian", "Carian", "Coptic Epact Numbers", "Old Italic",
	"Gothic", "Old Permic", "Ugaritic", "Old Persian", "Deseret", "Shavian", "Osmanya", "Osage", "Elbasan",
	"Caucasian Albanian", "Linear A", "Cypriot Syllabary", "Imperial Aramaic", "Palmyrene", "Nabataean", "Hatran",
	"Phoenician", "Lydian", "Meroitic Hieroglyphs", "Meroitic Cursive", "Kharoshthi", "Old South Arabian",
	"Old North Arabian", "Manichaean", "Avestan", "Inscriptional Parthian", "Inscriptional Pahlavi", "Psalter Pahlavi",
	"Old Turkic", "Old Hungarian", "Rumi Numeral Symbols", "Brahmi", "Kaithi", "Sora Sompeng", "Chakma", "Mahajani",
	"Sharada", "Sinhala Archaic Numbers", "Khojki", "Multani", "Khudawadi", "Grantha", "Newa", "Tirhuta", "Siddham",
	"Modi", "Mongolian Supplement", "Takri", "Ahom", "Warang Citi", "Zanabazar Square", "Soyombo", "Pau Cin Hau",
	"Bhaiksuki", "Marchen", "Masaram Gondi", "Cuneiform", "Cuneiform Numbers and Punctuation", "Early Dynastic Cuneiform",
	"Egyptian Hieroglyphs", "Anatolian Hieroglyphs", "Bamum Supplement", "Mro", "Bassa Vah", "Pahawh Hmong", "Miao",
	"Ideographic Symbols and Punctuation", "Tangut", "Tangut Components", "Kana Supplement", "Kana Extended-A", "Nushu",
	"Duployan", "Shorthand Format Controls", "Byzantine Musical Symbols", "Musical Symbols", "Ancient Greek Musical Notation",
	"Tai Xuan Jing Symbols", "Counting Rod Numerals", "Mathematical Alphanumeric Symbols", "Sutton SignWriting",
	"Glagolitic Supplement", "Mende Kikakui", "Adlam", "Arabic Mathematical Alphabetic Symbols", "Mahjong Tiles",
	"Domino Tiles", "Playing Cards", "Enclosed Alphanumeric Supplement", "Enclosed Ideographic Supplement",
	"Miscellaneous Symbols and Pictographs", "Emoticons", "Ornamental Dingbats", "Transport and Map Symbols",
	"Alchemical Symbols", "Geometric Shapes Extended", "Supplemental Arrows-C", "Supplemental Symbols and Pictographs",
	"CJK Unified Ideographs Extension B", "CJK Unified Ideographs Extension C", "CJK Unified Ideographs Extension D",
	"CJK Unified Ideographs Extension E", "CJK Unified Ideographs Extension F", "CJK Compatibility Ideographs Supplement",
	"Tags", "Variation Selectors Supplement", "Supplementary Private Use Area-A", "Supplementary Private Use Area-B",
	// Unicode 11.0.0
	"Georgian Extended", "Hanifi Rohingya", "Old Sogdian", "Sogdian", "Dogra", "Gunjala Gondi", "Makasar", "Medefaidrin",
	"Mayan Numerals", "Indic Siyaq Numbers", "Chess Symbols",
	// Unicode 12.0.0
	"Elymaic", "Nandinagari", "Tamil Supplement", "Egyptian Hieroglyph Format Controls", "Small Kana Extension",
	"Nyiakeng Puachue Hmong", "Wancho", "Ottoman Siyaq Numbers", "Symbols and Pictographs Extended-A",
	// Unicode 13.0.0
	"Yezidi", "Chorasmian", "Dives Akuru", "Lisu Supplement", "Khitan Small Script", "Tangut Supplement",
	"Symbols for Legacy Computing", "CJK Unified Ideographs Extension G",
	// Unicode 14.0.0
	"Arabic Extended-B", "Vithkuqi", "Latin Extended-F", "Old Uyghur", "Unified Canadian Aboriginal Syllabics Extended-A",
	"Cypro-Minoan", "Tangsa", "Kana Extended-B", "Znamenny Musical Notation", "Latin Extended-G", "Toto", "Ethiopic Extended-B",
	// Unicode 15.0.0
	"Arabic Extended-C", "Devanagari Extended-A", "Kawi", "Kaktovik Numerals", "Cyrillic Extended-D", "Nag Mundari",
	"CJK Unified Ideographs Extension H",
	// Unicode 15.1.0
	"CJK Unified Ideographs Extension I",
	// Unicode 16.0.0,
	"Todhri", "Garay", "Tulu-Tigalari", "Myanmar Extended-C", "Sunuwar", "Egyptian Hieroglyphs Extended-A", "Gurung Khema",
	"Kirat Rai", "Symbols for Legacy Computing Supplement", "Ol Onal"
};

static auto const blocks = build_namemap<enum_type::index, std::uint_least16_t>(block_names);

static std::vector<std::string> const age_names =
{
	"Unassigned", "1.1", "2.0", "2.1", "3.0", "3.1", "3.2", "4.0", "4.1", "5.0", "5.1", "5.2", "6.0", "6.1", "6.2", "6.3",
	"7.0", "8.0", "9.0", "10.0", "11.0", "12.0", "12.1", "13.0", "14.0", "15.0", "15.1", "16.0"
};

static auto const ages = build_namemap<enum_type::index, std::uint_least8_t>(age_names);

static std::vector<std::string> const eawidth_names = { "N", "A", "F", "H", "Na", "W" };

static auto const eawidths = build_namemap<enum_type::index, std::uint_least8_t>(eawidth_names);

template <class T> using ucd_array = std::array<T, 0x110000>;
static ucd_array<std::uint_least64_t> ptable; // Binary Properties table
static ucd_array<std::uint_least16_t> ctable; // POSIX Compatibility table
static ucd_array<std::uint_least8_t> gctable; // General Category table
static ucd_array<std::uint_least8_t> sctable; // Script table
static ucd_array<std::uint_least8_t> agetable; // Character age
static ucd_array<std::uint_least16_t> blocktable; // Block table
static ucd_array<std::uint_least8_t> widthtable; // Combined East Asian width and monospace column width table
static ucd_array<std::int_least32_t> cfoldtable; // Simple case folding offset table
static ucd_array<std::int_least32_t> clowertable; // Simple lowercase conversion offset table
static ucd_array<std::int_least32_t> cuppertable; // Simple uppercase conversion offset table

// Functions and data for loading in and parsing UCD files

constexpr auto default_rx_options = std::regex::ECMAScript | std::regex::optimize;
static std::regex const rx_ucd_version(R"(^#\s*\w+-(\d+(\.\d+)+).*)", default_rx_options);
static std::regex const rx_ucd_prop_range(R"(^\s*([0-9A-Fa-f]+)(\.\.([0-9A-Fa-f]+))?\s*;\s*([\w_.-]+(;\s+[\w_.-]+)?)\s*.*)", default_rx_options);
static std::regex const rx_ucd_case_folding(R"(^\s*([0-9A-Fa-f]+);\s*(C|S);\s*([0-9A-Fa-f]+);.*)", default_rx_options);

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
std::string read_ucd_array(stdfs::path filepath, std::regex const& line_regex, StoreFunc store)
{
	std::string version;
	std::ifstream input = open_ucd_file_and_read_version(filepath, version);
	try {
		std::string line;
		while (std::getline(input, line)) {
			std::smatch match;
			if (!line.empty() && line[0] != '#' && std::regex_match(line, match, line_regex)) {
				auto start = std::stoul(match.str(1), nullptr, 16);
				auto end = match.length(3) > 0 ? std::stoul(match.str(3), nullptr, 16) : start;
				if constexpr (std::is_invocable_v<StoreFunc, decltype(start), decltype(end)>)
					store(start, end);
				else
					store(start, end, match.str(4));
			}
		}
	} catch (std::exception& e) {
		throw makeunicode_error("unable to read data from " + filepath.string() + "\n -- " + e.what());
	}
	return version;
}

template <class StoreFunc>
std::string read_ucd_prop_array(stdfs::path filepath, StoreFunc store)
{
	return read_ucd_array(std::move(filepath), rx_ucd_prop_range, store);
}

template <class StoreFunc>
std::string read_ucd_case_array(stdfs::path filepath, StoreFunc store)
{
	return read_ucd_array(std::move(filepath), rx_ucd_case_folding, store);
}

// Filters for deriving POSIX compatibility table from binary properties and general
// categories, based on Unicode TR#18, Annex C: Compatibility Properties.

namespace compat_filter_arg {

constexpr std::size_t cflags = 0;
constexpr std::size_t pflags = 1;
constexpr std::size_t gcindex = 2;

} // namespace compat_filter_arg

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
			set_ptable_bits(0x00, 0x7f, "Ascii");
			set_ptable_bits(0x0a, 0x0d, "Line_Ending");
			set_ptable_bits(0x85, 0x85, "Line_Ending");
			set_ptable_bits(0x2028, 0x2029, "Line_Ending");
			auto proplist_version = read_ucd_prop_array("ucd/PropList.txt", set_ptable_bits);
			auto dcp_version = read_ucd_prop_array("ucd/DerivedCoreProperties.txt", set_ptable_bits);
			auto dbp_version = read_ucd_prop_array("ucd/extracted/DerivedBinaryProperties.txt", set_ptable_bits);
			return std::vector<std::string>{proplist_version, dcp_version, dbp_version};
		});

		// read in general category table
		auto pending_gcversion = std::async(std::launch::async, [] {
			std::fill(std::execution::par_unseq, gctable.begin(), gctable.end(), general_categories.find("Cn")->second);
			return read_ucd_prop_array("ucd/extracted/DerivedGeneralCategory.txt", [](auto start, auto end, auto const& value) {
				if (auto category = general_categories.find(value); category != general_categories.end())
					std::fill(std::execution::par_unseq, gctable.begin() + start, gctable.begin() + end + 1, category->second);
			});
		});

		// read in casefolding table
		auto pending_cfoldversion = std::async(std::launch::async, [] {
			std::fill(std::execution::par_unseq, cfoldtable.begin(), cfoldtable.end(), 0);
			return read_ucd_case_array("ucd/CaseFolding.txt", [](auto code, auto mapping) {
				if (cfoldtable[code] == 0)
					cfoldtable[code] = static_cast<std::int_least32_t>(mapping) - static_cast<std::int_least32_t>(code);
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

		// make sure reading of ptable and gctable is complete, as they're needed
		// to build the compatibility property table and lowercase/uppercase
		// mapping tables
		auto prop_versions = pending_prop_versions.get();
		prop_versions.push_back(pending_gcversion.get());
		prop_versions.push_back(pending_cfoldversion.get());

		// setup case conversion tables in parallel
		auto pending_caseconversion_task = std::async(std::launch::async, [] {
			auto const lowercase = binary_properties.find("Lowercase")->second;
			auto const uppercase = binary_properties.find("Uppercase")->second;
			auto const changes_on_lowercase = binary_properties.find("Changes_When_Lowercased")->second;
			auto const changes_on_uppercase = binary_properties.find("Changes_When_Uppercased")->second;
			std::fill(std::execution::par_unseq, clowertable.begin(), clowertable.end(), 0);
			std::fill(std::execution::par_unseq, cuppertable.begin(), cuppertable.end(), 0);
			// pass one, determine upper or lowercase conversions for target casefold mappings
			for (std::size_t code = 0, numcodes = cfoldtable.size(); code < numcodes; ++code) {
				if (auto const offset = cfoldtable[code]; offset != 0) {
					auto mapping = static_cast<std::size_t>(static_cast<std::ptrdiff_t>(code) + offset);
					assert((ptable[mapping] & (lowercase | uppercase)) != 0); // we assume target mappings aren't titlecase
					if ((ptable[mapping] & uppercase) != 0 && (ptable[code] & lowercase) != 0 && clowertable[mapping] == 0)
						clowertable[mapping] = -offset;
					if ((ptable[mapping] & lowercase) != 0 && (ptable[code] & uppercase) != 0 && cuppertable[mapping] == 0)
						cuppertable[mapping] = -offset;
				}
			}
			// pass two, determine upper or lowercase conversions for source casefold codes
			for (std::size_t code = 0, numcodes = cfoldtable.size(); code < numcodes; ++code) {
				if (auto const offset = cfoldtable[code]; offset != 0) {
					auto mapping = static_cast<std::size_t>(static_cast<std::ptrdiff_t>(code) + offset);
					auto mapping_lower = static_cast<std::size_t>(static_cast<std::ptrdiff_t>(mapping) + clowertable[mapping]);
					auto mapping_upper = static_cast<std::size_t>(static_cast<std::ptrdiff_t>(mapping) + cuppertable[mapping]);
					if (((ptable[code] & lowercase) == 0 || (ptable[code] & changes_on_lowercase) != 0) && clowertable[code] == 0)
						clowertable[code] = static_cast<std::int_least32_t>(mapping_lower) - static_cast<std::int_least32_t>(code);
					if (((ptable[code] & uppercase) == 0 || (ptable[code] & changes_on_uppercase) != 0) && cuppertable[code] == 0)
						cuppertable[code] = static_cast<std::int_least32_t>(mapping_upper) - static_cast<std::int_least32_t>(code);
				}
			}
		});

		// read in East Asian width and setup column width values in width table
		auto pending_eawidthversion = std::async(std::launch::async, [] {
			std::fill(std::execution::par_unseq, widthtable.begin(), widthtable.end(), eawidths.find("N")->second);
			auto width = read_ucd_case_array("ucd/EastAsianWidth.txt", [](auto start, auto end, auto const& value) {
				if (auto eawidth = eawidths.find(value); eawidth != eawidths.end())
					std::fill(std::execution::par_unseq, widthtable.begin() + start, widthtable.begin() + end + 1, eawidth->second);
			});
			auto const Me = general_categories.find("Me")->second;
			auto const Mn = general_categories.find("Mn")->second;
			auto const W = eawidths.find("W")->second;
			auto const F = eawidths.find("F")->second;
			for (std::size_t r = 0; r < widthtable.size(); ++r) {
				int colwidth = 1;
				if ((0x01 <= r && r <= 0x1f) || (0x7f <= r && r <= 0xa0))   // C0/C1 control characters
					colwidth = -1;
				else if (r == 0x00 || r == 0x034f ||                        // NULL, COMBINING GRAPHEME JOINER
							(0x1160 <= r && r <= 0x11ff) ||                 // Hangul Jamo medial vowels and final consonants
							(0x200b <= r && r <= 0x200f) ||                 // ZERO-WIDTH SPACE through RIGHT-TO-LEFT MARK
							(0x2028 <= r && r <= 0x202e) ||                 // LINE SEPARATOR through RIGHT-TO-LEFT OVERRIDE
							(0x2060 <= r && r <= 0x2063) ||                 // WORD JOINER through INVISIBLE SEPARATOR
							(r != 0x00ad && (                               // SOFT-HYPHEN
								gctable[r] == Me || gctable[r] == Mn)))     // Non-spacing characters
					colwidth = 0;
				else if (widthtable[r] == W || widthtable[r] == F)
					colwidth = 2;
				widthtable[r] |= static_cast<std::uint_least8_t>(colwidth + 1) << 4;
			}
			return width;
		});

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
							std::get<compat_filter_arg::cflags>(args) = static_cast<unsigned short>(std::get<compat_filter_arg::cflags>(args) | cflag);
					if (compat_prop_excludes.count(cname) != 0)
						for (auto const& exclude : compat_prop_excludes[cname])
							if (exclude(args))
								std::get<compat_filter_arg::cflags>(args) = static_cast<unsigned short>(std::get<compat_filter_arg::cflags>(args) & ~cflag);
				}
				ctable[i] = std::get<compat_filter_arg::cflags>(args);
			}
		);

		// wait for other tasks
		pending_caseconversion_task.wait();
		prop_versions.push_back(pending_eawidthversion.get());
		return prop_versions;
	});

	// load in the scripts table
	auto pending_scversion = std::async(std::launch::async, [] {
		std::fill(std::execution::par_unseq, sctable.begin(), sctable.end(), scripts.find("Unknown")->second);
		return read_ucd_prop_array("ucd/Scripts.txt", [](auto start, auto end, auto const& value) {
			if (auto script = scripts.find(value); script != scripts.end())
				std::fill(std::execution::par_unseq, sctable.begin() + start, sctable.begin() + end + 1, script->second);
		});
	});

	// load in the blocks table
	auto pending_blockversion = std::async(std::launch::async, [] {
		std::fill(std::execution::par_unseq, blocktable.begin(), blocktable.end(), blocks.find("No_block")->second);
		return read_ucd_prop_array("ucd/Blocks.txt", [](auto start, auto end, auto const& value) {
			if (auto block = blocks.find(value); block != blocks.end())
				std::fill(std::execution::par_unseq, blocktable.begin() + start, blocktable.begin() + end + 1, block->second);
		});
	});

	// load in the age table
	auto pending_ageversion = std::async(std::launch::async, [] {
		std::fill(std::execution::par_unseq, agetable.begin(), agetable.end(), ages.find("Unassigned")->second);
		return read_ucd_prop_array("ucd/DerivedAge.txt", [](auto start, auto end, auto const& value) {
			if (auto age = ages.find(value); age != ages.end())
				std::fill(std::execution::par_unseq, agetable.begin() + start, agetable.begin() + end + 1, age->second);
		});
	});

	// make sure versions from all ucd files match one another
	auto versions = pending_versions.get();
	versions.push_back(pending_scversion.get());
	versions.push_back(pending_blockversion.get());
	versions.push_back(pending_ageversion.get());
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

template <class T, class Types>
auto get_best_fit_type_info(T value, Types const& types)
{
	for (auto const& [min, max, name, cmacro, szbytes] : types)
		if (min <= value && value <= max)
			return ucd_type_info{name, cmacro, szbytes, static_cast<unsigned int>(std::to_string(value).size())};
	throw makeunicode_error("no best fit type");
}

template <class T>
auto get_best_fit_type_info(T value)
{
	static_assert(std::is_integral_v<T>, "best fit type must be integral");
	#define make_integral_type_entry(T, C) { (std::numeric_limits<T>::min)(), (std::numeric_limits<T>::max)(), #T, C, static_cast<unsigned int>(sizeof(T)) }
	if constexpr (std::is_signed_v<T>) {
		static std::vector<std::tuple<std::intmax_t, std::intmax_t, std::string_view, std::string_view, unsigned int>> const signed_types = {
			make_integral_type_entry(std::int_least8_t, "INT8_C"), make_integral_type_entry(std::int_least16_t, "INT16_C"),
			make_integral_type_entry(std::int_least32_t, "INT32_C"), make_integral_type_entry(std::int_least64_t, "INT64_C")
		};
		return get_best_fit_type_info(value, signed_types);
	} else {
		static std::vector<std::tuple<std::uintmax_t, std::uintmax_t, std::string_view, std::string_view, unsigned int>> const unsigned_types =
		{
			make_integral_type_entry(std::uint_least8_t, "UINT8_C"), make_integral_type_entry(std::uint_least16_t, "UINT16_C"),
			make_integral_type_entry(std::uint_least32_t, "UINT32_C"), make_integral_type_entry(std::uint_least64_t, "UINT64_C")
		};
		return get_best_fit_type_info(value, unsigned_types);
	}
	#undef make_integral_type_entry
}

inline auto get_table_type_info(std::vector<std::size_t> const& table)
{
	return get_best_fit_type_info(*std::max_element(table.cbegin(), table.cend()));
}

template <class T>
inline auto get_type_info()
{
	return get_best_fit_type_info((std::numeric_limits<T>::max)());
}

struct ucd_record
{
	std::uint_least64_t pflags;
	std::uint_least16_t cflags;
	std::uint_least16_t abfields;
	std::uint_least8_t gcindex;
	std::uint_least8_t scindex;
	std::uint_least8_t wfields;
	std::int_least32_t cfoffset;
	std::int_least32_t cloffset;
	std::int_least32_t cuoffset;
};

constexpr bool operator==(ucd_record const& x, ucd_record const& y) noexcept
{
	return x.pflags == y.pflags && x.cflags == y.cflags && x.abfields == y.abfields &&
		x.gcindex == y.gcindex && x.scindex == y.scindex && x.wfields == y.wfields &&
		x.cfoffset == y.cfoffset && x.cloffset == y.cloffset && x.cuoffset == y.cuoffset;
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
			std::hash<std::uint_least16_t>{}(record.abfields), std::hash<std::uint_least8_t>{}(record.gcindex),
			std::hash<std::uint_least8_t>{}(record.scindex),
			std::hash<std::uint_least8_t>{}(record.wfields), std::hash<std::int_least32_t>{}(record.cfoffset),
			std::hash<std::int_least32_t>{}(record.cloffset), std::hash<std::int_least32_t>{}(record.cuoffset));
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

template <class IndexMap, class ValueSequence, class Value>
static auto intern_value(IndexMap& indices, ValueSequence& values, Value value)
{
	using index_type = typename IndexMap::mapped_type;
	auto emplacement = indices.try_emplace(value, static_cast<index_type>(values.size()));
	if (emplacement.second) {
		assert(values.size() < (std::numeric_limits<index_type>::max)());
		values.push_back(value);
	}
	return emplacement.first;
}

struct ucd_flyweight_compressed_records
{
	std::unordered_map<std::uint_least64_t, std::uint_least8_t> pflag_indices;
	std::unordered_map<std::uint_least16_t, std::uint_least8_t> cflag_indices;
	std::unordered_map<std::int_least32_t, std::uint_least8_t> cmapping_indices;
	std::vector<std::uint_least64_t> pflag_values;
	std::vector<std::uint_least16_t> cflag_values;
	std::vector<std::int_least32_t> cmapping_values;
	std::vector<std::uint_least8_t> pflag_flyweights;
	std::vector<std::uint_least8_t> cflag_flyweights;
	std::vector<std::uint_least8_t> cfindices;
	std::vector<std::uint_least8_t> clindices;
	std::vector<std::uint_least8_t> cuindices;
	std::vector<std::uint_least8_t> gcindices;
	std::vector<std::uint_least8_t> scindices;
	std::vector<std::uint_least16_t> abfields;
	std::vector<std::uint_least8_t> wfields;

	ucd_flyweight_compressed_records() = default;

	explicit ucd_flyweight_compressed_records(std::vector<ucd_record> const& records) {
		pflag_flyweights.reserve(records.size());
		cflag_flyweights.reserve(records.size());
		cfindices.reserve(records.size());
		clindices.reserve(records.size());
		cuindices.reserve(records.size());
		gcindices.reserve(records.size());
		scindices.reserve(records.size());
		abfields.reserve(records.size());
		wfields.reserve(records.size());
		for (auto const& record : records) {
			pflag_flyweights.push_back(intern_value(pflag_indices, pflag_values, record.pflags)->second);
			cflag_flyweights.push_back(intern_value(cflag_indices, cflag_values, record.cflags)->second);
			cfindices.push_back(intern_value(cmapping_indices, cmapping_values, record.cfoffset)->second);
			clindices.push_back(intern_value(cmapping_indices, cmapping_values, record.cloffset)->second);
			cuindices.push_back(intern_value(cmapping_indices, cmapping_values, record.cuoffset)->second);
			gcindices.push_back(record.gcindex);
			scindices.push_back(record.scindex);
			abfields.push_back(record.abfields);
			wfields.push_back(record.wfields);
		}
	}
};

static std::size_t invalidrecordindex;
static std::unordered_map<ucd_record, std::size_t, ucd_record_hash> recordindices;
static std::vector<ucd_record> recordvalues;
static ucd_array<std::size_t> recordtable;
static ucd_record_stage_table recordstagetable;
static ucd_flyweight_compressed_records compressedrecords;

auto make_abfield(std::uint_least8_t age, std::uint_least16_t block)
{
	assert((age < ((1u << 6) - 1)) && (block <= ((1u << 10) - 1)));
	return static_cast<std::uint_least16_t>((static_cast<std::uint_least16_t>(age) << 10) | block);
}

std::size_t combine_record(ucd_record const& record)
{
	auto emplacement = recordindices.try_emplace(record, recordvalues.size());
	if (emplacement.second)
		recordvalues.push_back(record);
	return emplacement.first->second;
}

void combine_records()
{
	ucd_record invalidrecord{
		binary_properties.find("Any")->second, 0, make_abfield(ages.find("Unassigned")->second, blocks.find("No_block")->second),
		general_categories.find("Cn")->second, scripts.find("Unknown")->second, eawidths.find("N")->second,
		0, 0, 0};

	for (std::size_t i = 0, n = ptable.size(); i < n; ++i) {
		recordtable[i] = combine_record({
			ptable[i], ctable[i], make_abfield(agetable[i], blocktable[i]),
			gctable[i], sctable[i], widthtable[i],
			cfoldtable[i], clowertable[i], cuppertable[i]});
	}

	invalidrecordindex = combine_record(invalidrecord);
	compressedrecords = ucd_flyweight_compressed_records(recordvalues);
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
			auto blockfirst = blockit, blocklast = std::next(blockit, static_cast<std::ptrdiff_t>(trial.block_size));
			auto blockhash = std::accumulate(blockfirst, blocklast, std::size_t{0}, [](std::size_t x, std::size_t y) {
				return hash_combine(std::hash<std::size_t>{}(y), x);
			});
			auto [match, lastmatch] = stagedblockmap.equal_range(blockhash);
			for ( ; match != lastmatch; ++match) {
				if (std::equal(blockfirst, blocklast, std::next(trial.stage2.cbegin(), static_cast<std::ptrdiff_t>(match->second * trial.block_size))))
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

template <class T>
auto run_length_encode(std::vector<T> const& input, ucd_type_info const& info)
{
	// encode run-lengths
	T const maxseqlen = static_cast<T>((T{1} << ((CHAR_BIT * info.szbytes) - 2)) - 2);
	T const seqmask = T{3} << ((CHAR_BIT * info.szbytes) - 2);
	std::vector<T> pass1;

	for (auto curr = std::begin(input), last = std::end(input); curr != last; ) {
		T const value = *curr;
		auto const next = std::find_if_not(std::next(curr), last, [value](auto x) { return x == value; });
		auto count = static_cast<std::size_t>(std::distance(curr, next));
		if (count > 1 || (value & seqmask) == seqmask) {
			do {
				T seqlen = (std::min)(static_cast<T>(count - 1), maxseqlen);
				pass1.insert(std::end(pass1), {static_cast<T>(seqmask | seqlen), value});
				count -= static_cast<std::size_t>(seqlen) + 1;
			} while (count > maxseqlen);
		} else {
			pass1.push_back(value);
		}
		curr = next;
	}

	// encode interleave patterns
	T const ilseqcode = static_cast<T>((T{1} << (CHAR_BIT * info.szbytes)) - 1);
	T const maxilseqlen = ilseqcode - 1;
	std::vector<T> pass2;
	T lastval = 0;

	for (auto curr = std::begin(pass1), last = std::end(pass1); curr != last; ) {
		std::size_t count = 0;
		if (last - curr >= 4 && (curr[1] & seqmask) != seqmask && (lastval & seqmask) != seqmask)
			for (auto tail = curr + 2; last - tail >= 2 && count < maxilseqlen && curr[0] == tail[0] && curr[1] == tail[1]; tail += 2)
				++count;
		if (count > 1) {
			pass2.insert(std::end(pass2), {ilseqcode, static_cast<T>(count + 1), curr[0], curr[1]});
			curr += static_cast<std::ptrdiff_t>((count + 1) * 2);
			lastval = 0;
		} else {
			pass2.push_back(*curr++);
			lastval = pass2.back();
		}
	}

	return pass2;
}

template <class T>
auto run_length_encode(std::vector<T> const& input)
{
	return run_length_encode(input, get_type_info<T>());
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

auto make_version_identifier(std::string_view version)
{
	if (!std::isdigit(version.front()))
		return std::string{version};
	std::string versionid;
	versionid.push_back('v');
	for (char c : version)
		versionid.push_back(c == '.' ? '_' : c);
	return versionid;
}

auto normalize_property_identifier(std::string_view id)
{
	std::string normid;
	for (char c : id)
		if (c != ';')
			normid.push_back(c == ' ' || c == '\t' || c == '-' || c == '.' ? '_' : c);
	return normid;
}

auto normalize_property_label(std::string_view label)
{
	std::string normlabel;
	for (char c : label)
		if (c != ' ' && c != '\t' && c != '_' && c != '-' && c != '.' && c != ';')
			normlabel.push_back(static_cast<char>(std::tolower(c)));
	return normlabel;
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

template <class ValueSequence>
struct function_table_printer
{
	std::string_view name_;
	std::string_view type_;
	ValueSequence const& values_;

public:
	function_table_printer(std::string_view name, std::string_view type, ValueSequence const& values)
		: name_{name}, type_{type}, values_{values} {}

	friend std::ostream& operator<<(std::ostream& out, function_table_printer const& p) {
		return print_table(out, p.name_, p.type_, "", p.values_, 120, "\t", "\n\t\t");
	}
};

template <class T>
class comma_printer
{
	T index_;
	T count_;
	std::string_view newline_;

public:
	comma_printer(T index, T count, std::string_view newline = "\n")
		: index_{index}, count_{count}, newline_{newline} {}

	friend std::ostream& operator<<(std::ostream& out, comma_printer const& p) {
		if (p.index_ < (p.count_ - 1))
			out << ',';
		if (!p.newline_.empty())
			out << p.newline_;
		return out;
	}
};

template <class T> comma_printer(T, T) -> comma_printer<T>;
template <class T> comma_printer(T, T, std::string_view) -> comma_printer<T>;

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
		p.body_(out);
		return out << "};\n";
	}
};

class enum_parser_printer
{
	std::string_view name_;
	std::string_view abbr_;
	std::size_t maxcolwidth_{120};
	std::function<std::vector<std::pair<std::string, std::string>>()> label_source_;

public:
	template <class LabelSource>
	enum_parser_printer(std::string_view name, std::string_view abbr, LabelSource label_source)
		: name_{name}, abbr_{abbr}, label_source_{std::move(label_source)} {}

	template <class LabelSource>
	enum_parser_printer(std::string_view name, std::string_view abbr, std::size_t colwidth, LabelSource label_source)
		: name_{name}, abbr_{abbr}, maxcolwidth_{colwidth}, label_source_{std::move(label_source)} {}

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
		line.reserve(p.maxcolwidth_ * 2);
		segment.reserve(80);

		for (std::size_t i = 0, n = labels.size(); i < n; ++i) {
			segment = "{ \"" + labels[i].first + "\"sv, ";
			segment.append(p.abbr_.data(), p.abbr_.size());
			segment += "::" + labels[i].second + " }";
			if (i < n - 1)
				segment += ',';
			if (segment.size() + line.size() > p.maxcolwidth_) {
				out << "\t\t" << line << "\n";
				line = segment;
				continue;
			}
			if (i > 0)
				line += ' ';
			line += segment;
		}

		return out << "\t\t" << line << "\n"
			<< "\t} };\n\n"
			<< "\tauto const l = detail::normalize_property_label(s);\n"
			<< "\tauto const c = std::lower_bound(labels.begin(), labels.end(), l, [](auto const& x, auto const& y) { return x.first < y; });\n"
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
	ucd_flyweight_compressed_records const& records_;

public:
	explicit record_flyweight_printer(ucd_flyweight_compressed_records const& records)
		: records_{records} {}

	friend std::ostream& operator<<(std::ostream& out, record_flyweight_printer const& p) {
		print_table(out, "rlepflagindices", "std::uint_least8_t", "", run_length_encode(p.records_.pflag_flyweights), 120, "\t", "\n\t\t") << "\n";
		print_table(out, "rlecflagindices", "std::uint_least8_t", "", run_length_encode(p.records_.cflag_flyweights), 120, "\t", "\n\t\t") << "\n";
		print_table(out, "rleabfields", "std::uint_least16_t", "", run_length_encode(p.records_.abfields), 120, "\t", "\n\t\t") << "\n";
		print_table(out, "rlegcindices", "std::uint_least8_t", "", run_length_encode(p.records_.gcindices), 120, "\t", "\n\t\t") << "\n";
		print_table(out, "rlescindices", "std::uint_least8_t", "", run_length_encode(p.records_.scindices), 120, "\t", "\n\t\t") << "\n";
		print_table(out, "rlewfields", "std::uint_least8_t", "", run_length_encode(p.records_.wfields), 120, "\t", "\n\t\t") << "\n";
		print_table(out, "rlecfindices", "std::uint_least8_t", "", run_length_encode(p.records_.cfindices), 122, "\t", "\n\t\t") << "\n";
		print_table(out, "rleclindices", "std::uint_least8_t", "", run_length_encode(p.records_.clindices), 122, "\t", "\n\t\t") << "\n";
		print_table(out, "rlecuindices", "std::uint_least8_t", "", run_length_encode(p.records_.cuindices), 122, "\t", "\n\t\t") << "\n";
		print_table(out, "pflags", "std::uint_least64_t", "UINT64_C", p.records_.pflag_values, 120, "\t", "\n\t\t") << "\n";
		print_table(out, "cflags", "std::uint_least16_t", "", p.records_.cflag_values, 120, "\t", "\n\t\t");
		return out;
	}
};

void print_unicode_header()
{

double log2_block_size = std::ceil(std::log2(static_cast<double>(recordstagetable.block_size)));
std::size_t const block_shift = static_cast<std::size_t>(std::lrint(log2_block_size));
std::size_t const block_mask = (std::size_t{1} << block_shift) - 1;

std::cout <<
R"c++(// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

// This header file is generated by the makeunicode tool program.
// Do not modify this file by hand. Instead, modify and run the
// tool to regenerate this file.

#ifndef LUG_INCLUDE_LUG_UNICODE_HPP
#define LUG_INCLUDE_LUG_UNICODE_HPP

#include <lug/detail.hpp>

#include <cctype>
#include <cstdint>

#include <array>
#include <bitset>
#include <memory>
#include <optional>

namespace lug::unicode {

static constexpr char32_t ascii_limit = 0x80U;

[[nodiscard]] constexpr bool is_ascii(char32_t r) noexcept
{
	return r < ascii_limit;
}

// NOLINTBEGIN(hicpp-signed-bitwise)
)c++"
<< "\n"
<< enum_printer(enum_type::bitfield, "ctype", "std::uint_least16_t", "POSIX compatibility properties", [](std::ostream& out) {
	auto const& cnames = compatibility_property_names;
	auto const pad = align_padding(max_element_size(cnames.cbegin(), cnames.cend()));
	out << "\t" << std::left << std::setw(pad) << "none" << " = 0,\n";
	for (std::size_t i = 0, n = cnames.size(); i < n; ++i)
		out << "\t" << std::left << std::setw(pad) << cnames[i] << " = UINT16_C(1) << " << std::right << std::setw(2) << i << comma_printer{i, n};
})
<< "\n"
<< enum_printer(enum_type::bitfield, "ptype", "std::uint_least64_t", "Binary properties", [](std::ostream& out) {
	auto const& pnames = binary_property_names;
	auto const pad = align_padding(max_element_size(pnames.cbegin(), pnames.cend()));
	out << "\t" << std::left << std::setw(pad) << "None" << " = 0,\n";
	for (std::size_t i = 0, n = pnames.size(); i < n; ++i)
		out << "\t" << std::left << std::setw(pad) << normalize_property_identifier(pnames[i]) << " = UINT64_C(1) << " << std::right << std::setw(2) << i << comma_printer{i, n};
})
<< "\n"
<< enum_printer(enum_type::bitfield, "gctype", "std::uint_least32_t", "General categories", [](std::ostream& out) {
	auto const& gcnames = general_category_names;
	auto const& gclnames = general_category_long_names;
	out << "\t" << "None = 0,\n";
	for (std::size_t i = 0, n = gcnames.size(); i < n; ++i)
		out << "\t" << gcnames[i] << " = UINT32_C(1) << " << std::right << std::setw(2) << i << ",    " << gclnames[i] << " = " << gcnames[i] << ",\n";
	std::size_t const compound_count = compound_general_categories.size();
	std::size_t compound_index = 0;
	for (auto const& compound : compound_general_categories) {
		out << "\t" << std::left << std::setw(2) << compound.first << " = ";
		auto const& components = compound.second.second;
		int padcount = 0;
		for (std::size_t j = 0, m = components.size(); j < m; ++j) {
			out << components[j];
			padcount += static_cast<int>(components[j].size());
			if (j < (m - 1)) {
				out << "|";
				++padcount;
			}
		}
		out << "," << std::right << std::setw(21 - padcount) << " " << compound.second.first << " = " << compound.first << comma_printer{compound_index, compound_count};
		++compound_index;
	}
})
<< "\n// NOLINTEND(hicpp-signed-bitwise)\n\n"
<< enum_printer(enum_type::index, "sctype", "std::uint_least8_t", "Scripts", [](std::ostream& out) {
	auto const pad = align_padding(max_element_size(script_names.cbegin(), script_names.cend()));
	for (std::size_t i = 0, n = script_names.size(); i < n; ++i)
		out << "\t" << std::left << std::setw(pad) << script_names[i] << " = " << std::right << std::setw(3) << i << comma_printer{i, n};
})
<< "\n"
<< enum_printer(enum_type::index, "blktype", "std::uint_least16_t", "Blocks", [](std::ostream& out) {
	std::vector<std::string> names;
	names.reserve(block_names.size());
	std::transform(std::cbegin(block_names), std::cend(block_names), std::back_inserter(names), normalize_property_identifier);
	auto const pad = align_padding(max_element_size(std::begin(names), std::end(names)));
	for (std::size_t i = 0, n = names.size(); i < n; ++i)
		out << "\t" << std::left << std::setw(pad) << names[i] << " = " << std::right << std::setw(3) << i << comma_printer{i, n};
})
<< "\n"
<< enum_printer(enum_type::index, "agetype", "std::uint_least8_t", "Character Age", [](std::ostream& out) {
	std::vector<std::string> age_enums;
	std::transform(age_names.begin(), age_names.end(), std::back_inserter(age_enums), [](auto& name) { return make_version_identifier(name); });
	auto const pad = align_padding(max_element_size(age_enums.cbegin(), age_enums.cend()));
	for (std::size_t i = 0, n = age_enums.size(); i < n; ++i)
		out << "\t" << std::left << std::setw(pad) << age_enums[i] << " = " << std::right << std::setw(3) << i << comma_printer{i, n};
})
<< "\n"
<< enum_printer(enum_type::index, "eawtype", "std::uint_least8_t", "East Asian Width", [](std::ostream& out) {
	auto const pad = align_padding(max_element_size(eawidth_names.cbegin(), eawidth_names.cend()));
	for (std::size_t i = 0, n = eawidth_names.size(); i < n; ++i)
		out << "\t" << std::left << std::setw(pad) << eawidth_names[i] << " = " << std::right << std::setw(3) << i << comma_printer{i, n};
})
<< R"c++(
// Property Traits
enum class property_enum : std::uint_least8_t
{
	invalid,
	ctype,
	ptype,
	gctype,
	sctype,
	blktype,
	agetype,
	eawtype
};

template <class T> inline constexpr property_enum to_property_enum_v = property_enum::invalid;
template <> inline constexpr property_enum to_property_enum_v<ctype> = property_enum::ctype;
template <> inline constexpr property_enum to_property_enum_v<ptype> = property_enum::ptype;
template <> inline constexpr property_enum to_property_enum_v<gctype> = property_enum::gctype;
template <> inline constexpr property_enum to_property_enum_v<sctype> = property_enum::sctype;
template <> inline constexpr property_enum to_property_enum_v<blktype> = property_enum::blktype;
template <> inline constexpr property_enum to_property_enum_v<agetype> = property_enum::agetype;
template <> inline constexpr property_enum to_property_enum_v<eawtype> = property_enum::eawtype;

template <class T> inline constexpr bool is_property_enum_v = to_property_enum_v<std::decay_t<T>> != property_enum::invalid;

} // namespace lug::unicode

template <> inline constexpr bool lug::is_flag_enum_v<lug::unicode::ctype> = true;
template <> inline constexpr bool lug::is_flag_enum_v<lug::unicode::ptype> = true;
template <> inline constexpr bool lug::is_flag_enum_v<lug::unicode::gctype> = true;

namespace lug::unicode {

// NOLINTBEGIN(cppcoreguidelines-avoid-magic-numbers,readability-magic-numbers)

// Unicode Character Database record
class record
{
	struct raw_record {
		std::uint_least64_t pflags;
		std::uint_least16_t cflags;
		std::uint_least16_t abfields;
		std::uint_least8_t gcindex;
		std::uint_least8_t scindex;
		std::uint_least8_t wfields;
		std::uint_least8_t cfindex;
		std::uint_least8_t clindex;
		std::uint_least8_t cuindex;
	} const* raw_;
	explicit record(raw_record const* r) noexcept : raw_(r) {}
	struct raw_record_table {)c++"
		<< "\n\t\tstd::array<" << recordstagetable.typeinfo1.name << ", " << std::dec << recordstagetable.stage1.size() << "> stage1;\n"
		<< "\t\tstd::array<" << recordstagetable.typeinfo2.name << ", " << std::dec << recordstagetable.stage2.size() << "> stage2;\n"
		<< "\t\tstd::array<raw_record, " << std::dec << recordvalues.size() << "> records;" << R"c++(
	};
	[[nodiscard]] static std::int_least32_t case_mapping(std::size_t index) noexcept;
	[[nodiscard]] static std::unique_ptr<raw_record_table> decompress_table();
	friend record query(char32_t r);
public:
	[[nodiscard]] ctype compatibility() const noexcept { return static_cast<ctype>(raw_->cflags); }
	[[nodiscard]] ptype properties() const noexcept { return static_cast<ptype>(raw_->pflags); }
	[[nodiscard]] gctype general_category() const noexcept { return static_cast<gctype>(UINT32_C(1) << raw_->gcindex); }
	[[nodiscard]] sctype script() const noexcept { return static_cast<sctype>(raw_->scindex); }
	[[nodiscard]] blktype block() const noexcept { return static_cast<blktype>(raw_->abfields & 0x03ffU); }
	[[nodiscard]] agetype age() const noexcept { return static_cast<agetype>(raw_->abfields >> 10U); }
	[[nodiscard]] eawtype eawidth() const noexcept { return static_cast<eawtype>(raw_->wfields & 0x0fU); }
	[[nodiscard]] int cwidth() const noexcept { return static_cast<int>(raw_->wfields >> 4U) - 1; }
	[[nodiscard]] std::int_least32_t casefold_mapping() const noexcept { return case_mapping(raw_->cfindex); }
	[[nodiscard]] std::int_least32_t lowercase_mapping() const noexcept { return case_mapping(raw_->clindex); }
	[[nodiscard]] std::int_least32_t uppercase_mapping() const noexcept { return case_mapping(raw_->cuindex); }
	[[nodiscard]] bool all_of(ctype c) const noexcept { return (compatibility() & c) == c; }
	[[nodiscard]] bool all_of(ptype p) const noexcept { return (properties() & p) == p; }
	[[nodiscard]] bool all_of(gctype gc) const noexcept { return (general_category() & gc) == gc; }
	[[nodiscard]] bool any_of(ctype c) const noexcept { return (compatibility() & c) != ctype::none; }
	[[nodiscard]] bool any_of(ptype p) const noexcept { return (properties() & p) != ptype::None; }
	[[nodiscard]] bool any_of(gctype gc) const noexcept { return (general_category() & gc) != gctype::None; }
	[[nodiscard]] bool none_of(ctype c) const noexcept { return (compatibility() & c) == ctype::none; }
	[[nodiscard]] bool none_of(ptype p) const noexcept { return (properties() & p) == ptype::None; }
	[[nodiscard]] bool none_of(gctype gc) const noexcept { return (general_category() & gc) == gctype::None; }
};

// Retrieves the UCD record for the given codepoint
[[nodiscard]] inline record query(char32_t r)
{
	static auto const table = record::decompress_table();
	std::size_t index = )c++" << std::dec << invalidrecordindex << R"c++(;
	if (r < 0x)c++" << std::hex << ptable.size() << R"c++() {
		index = table->stage1[r >> )c++" << std::dec << block_shift << R"c++(U];
		index = table->stage2[(index << )c++" << std::dec << block_shift << R"c++(U) | (r & 0x)c++" << std::hex << block_mask << R"c++(U)];
	}
	return record{&table->records[index]};
}

struct all_of_fn
{
	[[nodiscard]] bool operator()(record const& rec, property_enum penum, std::uint_least64_t pflags) const noexcept
	{
		switch (penum) {
			case property_enum::ctype: return rec.all_of(static_cast<ctype>(pflags));
			case property_enum::ptype: return rec.all_of(static_cast<ptype>(pflags));
			case property_enum::gctype: return rec.all_of(static_cast<gctype>(pflags));
			case property_enum::sctype: return rec.script() == static_cast<sctype>(pflags);
			case property_enum::blktype: return rec.block() == static_cast<blktype>(pflags);
			case property_enum::agetype: return rec.age() == static_cast<agetype>(pflags);
			case property_enum::eawtype: return rec.eawidth() == static_cast<eawtype>(pflags);
			case property_enum::invalid: return false;
		}
		return false;
	}
};

// Checks if the rune matches all of the string-packed property classes
inline constexpr all_of_fn all_of{};

struct any_of_fn
{
	[[nodiscard]] bool operator()(record const& rec, property_enum penum, std::uint_least64_t pflags) const noexcept
	{
		switch (penum) {
			case property_enum::ctype: return rec.any_of(static_cast<ctype>(pflags));
			case property_enum::ptype: return rec.any_of(static_cast<ptype>(pflags));
			case property_enum::gctype: return rec.any_of(static_cast<gctype>(pflags));
			case property_enum::sctype: return rec.script() == static_cast<sctype>(pflags);
			case property_enum::blktype: return rec.block() == static_cast<blktype>(pflags);
			case property_enum::agetype: return rec.age() == static_cast<agetype>(pflags);
			case property_enum::eawtype: return rec.eawidth() == static_cast<eawtype>(pflags);
			case property_enum::invalid: return false;
		}
		return false;
	}
};

// Checks if the rune matches any of the string-packed property classes
inline constexpr any_of_fn any_of{};

struct none_of_fn
{
	[[nodiscard]] bool operator()(record const& rec, property_enum penum, std::uint_least64_t pflags) const noexcept
	{
		switch (penum) {
			case property_enum::ctype: return rec.none_of(static_cast<ctype>(pflags));
			case property_enum::ptype: return rec.none_of(static_cast<ptype>(pflags));
			case property_enum::gctype: return rec.none_of(static_cast<gctype>(pflags));
			case property_enum::sctype: return rec.script() != static_cast<sctype>(pflags);
			case property_enum::blktype: return rec.block() != static_cast<blktype>(pflags);
			case property_enum::agetype: return rec.age() != static_cast<agetype>(pflags);
			case property_enum::eawtype: return rec.eawidth() != static_cast<eawtype>(pflags);
			case property_enum::invalid: return false;
		}
		return false;
	}
};

// Checks if the rune matches none of the string-packed property classes
inline constexpr none_of_fn none_of{};

// Column width (-1 = non-displayable, 0 = non-spacing, 1 = normal, 2 = wide)
[[nodiscard]] inline int cwidth(char32_t r)
{
	return query(r).cwidth();
}

// Absolute column width
[[nodiscard]] inline unsigned int ucwidth(char32_t r)
{
	auto const cw = query(r).cwidth();
	return static_cast<unsigned int>(cw >= 0 ? cw : -cw);
}

// Simple casefold conversion
[[nodiscard]] inline char32_t tocasefold(char32_t r)
{
	return static_cast<char32_t>(static_cast<std::int_least32_t>(r) + query(r).casefold_mapping());
}

// Simple lowercase conversion
[[nodiscard]] inline char32_t tolower(char32_t r)
{
	return static_cast<char32_t>(static_cast<std::int_least32_t>(r) + query(r).lowercase_mapping());
}

// Simple uppercase conversion
[[nodiscard]] inline char32_t toupper(char32_t r)
{
	return static_cast<char32_t>(static_cast<std::int_least32_t>(r) + query(r).uppercase_mapping());
}

// Sparse character rune set
class rune_set
{
	friend rune_set negate(rune_set const& /*unused*/);
	friend rune_set sort_and_optimize(rune_set /*unused*/);

	std::vector<std::pair<char32_t, char32_t>> intervals;
	std::bitset<128> ascii;

	rune_set(std::vector<std::pair<char32_t, char32_t>>&& intr, std::bitset<128> const& asc)
		: intervals{std::move(intr)}, ascii{asc} {}

	void push_uniform_casefolded_range(ptype props, char32_t start, char32_t end)
	{
		if ((props & ptype::Cased) != ptype::None) {
			push_range(unicode::tolower(start), unicode::tolower(end));
			push_range(unicode::toupper(start), unicode::toupper(end));
		} else {
			push_range(start, end);
		}
	}

public:
	rune_set() = default;
	rune_set(rune_set const&) = default;
	rune_set(rune_set&&) = default;
	rune_set& operator=(rune_set const&) = default;
	rune_set& operator=(rune_set&&) = default;
	~rune_set() = default;

	rune_set(std::initializer_list<char32_t> list)
	{
		for (char32_t const r : list)
			push_range(r, r);
	}

	[[nodiscard]] bool operator==(rune_set const& rhs) const noexcept { return (ascii == rhs.ascii) && (intervals == rhs.intervals); }
	[[nodiscard]] bool operator!=(rune_set const& rhs) const noexcept { return !(*this == rhs); }
	[[nodiscard]] bool empty() const noexcept { return intervals.empty() && ascii.none(); }
	void clear() noexcept { intervals.clear(); ascii.reset(); }

	[[nodiscard]] bool contains(char32_t rune) const noexcept
	{
		if (rune < unicode::ascii_limit)
			return ascii[static_cast<std::size_t>(rune)];
		auto const interval = std::lower_bound(intervals.begin(), intervals.end(), rune, [](auto const& x, auto const& y) noexcept { return x.second < y; });
		return (interval != intervals.end()) && (interval->first <= rune) && (rune <= interval->second);
	}

	void push_rune(char32_t rune)
	{
		if (rune < unicode::ascii_limit) {
			ascii.set(static_cast<std::size_t>(rune));
		} else {
			intervals.emplace_back(rune, rune);
			std::push_heap(std::begin(intervals), std::end(intervals));
		}
	}

	void push_range(char32_t start, char32_t end)
	{
		if (start > end)
			throw bad_character_range{};
		for (char32_t rn = start; rn <= end && rn < unicode::ascii_limit; ++rn)
			ascii.set(static_cast<std::size_t>(rn));
		if (end >= unicode::ascii_limit) {
			intervals.emplace_back((std::max)(start, unicode::ascii_limit), end);
			std::push_heap(std::begin(intervals), std::end(intervals));
		}
	}

	void push_casefolded_range(char32_t start, char32_t end)
	{
		if (start > end)
			throw bad_character_range{};
		ptype p = query(start).properties();
		char32_t r1 = start;
		char32_t r2 = start;
		for (char32_t rn = start + 1; rn <= end; r2 = rn, ++rn) {
			ptype const q = query(start).properties();
			if (((p ^ q) & ptype::Cased) != ptype::None) {
				push_uniform_casefolded_range(p, r1, r2);
				r1 = rn;
				p = q;
			}
		}
		push_uniform_casefolded_range(p, r1, r2);
	}
};

[[nodiscard]] inline rune_set negate(rune_set const& runes)
{
	rune_set negated_runes;
	if (!runes.intervals.empty()) {
		if (char32_t const front = runes.intervals.front().first; unicode::ascii_limit < front)
			negated_runes.intervals.emplace_back(unicode::ascii_limit, front - 1);
		if (runes.intervals.size() > 1) {
			auto const last = std::cend(runes.intervals);
			auto left = std::cbegin(runes.intervals);
			for (;;) {
				auto right = std::next(left);
				if (right == last)
					break;
				negated_runes.intervals.emplace_back(left->second + 1, right->first - 1);
				left = right;
			}
		}
		if (char32_t const back = runes.intervals.back().second; back < U'\xFFFFFFFF')
			negated_runes.intervals.emplace_back(back + 1, U'\xFFFFFFFF');
	} else {
		negated_runes.intervals.emplace_back(unicode::ascii_limit, U'\xFFFFFFFF');
	}
	negated_runes.intervals.shrink_to_fit();
	negated_runes.ascii = ~runes.ascii;
	return negated_runes;
}

[[nodiscard]] inline rune_set sort_and_optimize(rune_set runes)
{
	if (runes.intervals.empty())
		return runes;
	std::vector<std::pair<char32_t, char32_t>> optimized_intervals;
	auto out = optimized_intervals.end();
	std::sort_heap(std::begin(runes.intervals), std::end(runes.intervals));
	for (auto const& r : runes.intervals) {
		if (out == optimized_intervals.end() || r.first < out->first || out->second < r.first)
			out = optimized_intervals.insert(optimized_intervals.end(), r);
		else
			out->second = out->second < r.second ? r.second : out->second;
	}
	optimized_intervals.shrink_to_fit();
	return rune_set{std::move(optimized_intervals), runes.ascii};
}

namespace detail {

[[nodiscard]] inline std::string normalize_property_label(std::string_view id)
{
	std::string normid;
	for (char const c : id)
		if (c != ' ' && c != '\t' && c != '_' && c != '-' && c != '.' && c != ';')
			normid.push_back(static_cast<char>(std::tolower(c)));
	return normid;
}

} // namespace detail

)c++" << enum_parser_printer("ctype", "ct", [] {
	auto const& cnames = compatibility_property_names;
	std::vector<std::pair<std::string, std::string>> labels;
	std::transform(cnames.begin(), cnames.end(), std::back_inserter(labels), [](auto& label) {
		return std::make_pair(normalize_property_label(label), label);
	});
	return labels;
})
<< "\n"
<< enum_parser_printer("ptype", "pt", [] {
	std::vector<std::pair<std::string, std::string>> labels;
	std::transform(binary_property_names.begin(), binary_property_names.end(), std::back_inserter(labels), [](auto& label) {
		return std::make_pair(normalize_property_label(label), normalize_property_identifier(label));
	});
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
	std::transform(script_names.begin(), script_names.end(), std::back_inserter(labels), [](auto& label) {
		return std::make_pair(normalize_property_label(label), label);
	});
	return labels;
})
<< "\n"
<< enum_parser_printer("blktype", "blk", 138, [] {
	std::vector<std::pair<std::string, std::string>> labels;
	std::transform(block_names.begin(), block_names.end(), std::back_inserter(labels), [](auto& label) {
		return std::make_pair(normalize_property_label(label), normalize_property_identifier(label));
	});
	return labels;
})
<< "\n"
<< enum_parser_printer("agetype", "at", [] {
	std::vector<std::pair<std::string, std::string>> labels;
	std::transform(age_names.begin(), age_names.end(), std::back_inserter(labels), [](auto& label) {
		return std::make_pair(normalize_property_label(label), make_version_identifier(label));
	});
	return labels;
})
<< "\n"
<< enum_parser_printer("eawtype", "eaw", [] {
	std::vector<std::pair<std::string, std::string>> labels;
	std::transform(eawidth_names.begin(), eawidth_names.end(), std::back_inserter(labels), [](auto& label) {
		return std::make_pair(normalize_property_label(label), label);
	});
	return labels;
})
<< R"c++(
namespace detail {

template <class InputIt, class OutputIt>
void run_length_decode(InputIt first, InputIt last, OutputIt dest)
{
	using value_type = typename std::iterator_traits<InputIt>::value_type;
	constexpr auto ilseqcode = (std::numeric_limits<value_type>::max)();
	constexpr auto seqmask = static_cast<value_type>(0x03ULL << static_cast<unsigned int>(std::numeric_limits<value_type>::digits - 2));
	while (first != last) {
		if (auto const lead = *first++; lead == ilseqcode) {
			auto const count = static_cast<std::size_t>(*first++);
			auto const head = *first++;
			auto const tail = *first++;
			for (std::size_t i = 0; i < count; ++i) {
				if ((head & seqmask) == seqmask) {
					dest = std::fill_n(dest, static_cast<std::size_t>(head & ~static_cast<std::size_t>(seqmask)) + 1, tail);
				} else {
					*dest++ = head;
					*dest++ = tail;
				}
			}
		} else if ((lead & seqmask) == seqmask) {
			dest = std::fill_n(dest, static_cast<std::size_t>(lead & ~static_cast<std::size_t>(seqmask)) + 1, *first++);
		} else {
			*dest++ = lead;
		}
	}
}

} // namespace detail

[[nodiscard]] inline std::int_least32_t record::case_mapping(std::size_t index) noexcept
{
)c++"
<< function_table_printer<decltype(compressedrecords.cmapping_values)>("casemappings", "std::int_least32_t", compressedrecords.cmapping_values)
<< R"c++(
	return casemappings[index];
}

[[nodiscard]] inline std::unique_ptr<record::raw_record_table> record::decompress_table()
{
	using detail::run_length_decode;
	using lug::detail::make_member_accessor;

)c++"
<< rle_stage_table_printer("rlestage1", recordstagetable.stage1, recordstagetable.typeinfo1) << "\n"

<< rle_stage_table_printer("rlestage2", recordstagetable.stage2, recordstagetable.typeinfo2) << "\n"

<< record_flyweight_printer(compressedrecords)
<< "\n\tstd::array<std::uint_least8_t, " << compressedrecords.pflag_flyweights.size() << "> flyweights{};"
<< R"c++(
	auto table = std::make_unique<raw_record_table>();
	auto& records = table->records;

	run_length_decode(std::cbegin(rlestage1), std::cend(rlestage1), std::begin(table->stage1));
	run_length_decode(std::cbegin(rlestage2), std::cend(rlestage2), std::begin(table->stage2));

	run_length_decode(std::cbegin(rlepflagindices), std::cend(rlepflagindices), std::begin(flyweights));
	for (std::size_t r = 0, e = records.size(); r < e; ++r)
		records[r].pflags = pflags[flyweights[r]];

	run_length_decode(std::cbegin(rlecflagindices), std::cend(rlecflagindices), std::begin(flyweights));
	for (std::size_t r = 0, e = records.size(); r < e; ++r)
		records[r].cflags = cflags[flyweights[r]];

	run_length_decode(std::cbegin(rleabfields), std::cend(rleabfields),
		make_member_accessor<decltype(&raw_record::abfields), &raw_record::abfields>(std::begin(records)));
	run_length_decode(std::cbegin(rlegcindices), std::cend(rlegcindices),
		make_member_accessor<decltype(&raw_record::gcindex), &raw_record::gcindex>(std::begin(records)));
	run_length_decode(std::cbegin(rlescindices), std::cend(rlescindices),
		make_member_accessor<decltype(&raw_record::scindex), &raw_record::scindex>(std::begin(records)));
	run_length_decode(std::cbegin(rlewfields), std::cend(rlewfields),
		make_member_accessor<decltype(&raw_record::wfields), &raw_record::wfields>(std::begin(records)));
	run_length_decode(std::cbegin(rlecfindices), std::cend(rlecfindices),
		make_member_accessor<decltype(&raw_record::cfindex), &raw_record::cfindex>(std::begin(records)));
	run_length_decode(std::cbegin(rleclindices), std::cend(rleclindices),
		make_member_accessor<decltype(&raw_record::clindex), &raw_record::clindex>(std::begin(records)));
	run_length_decode(std::cbegin(rlecuindices), std::cend(rlecuindices),
		make_member_accessor<decltype(&raw_record::cuindex), &raw_record::cuindex>(std::begin(records)));

	return table;
}

// NOLINTEND(cppcoreguidelines-avoid-magic-numbers,readability-magic-numbers)

} // namespace lug::unicode

#endif
)c++";

} // void print_unicode_header()

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
