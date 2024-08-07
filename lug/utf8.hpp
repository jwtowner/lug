// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2024 Jesse W. Towner
// See LICENSE.md file for license details

// Based on the Flexible and Economical UTF-8 Decoder by Bjoern Hoehrmann
// Copyright (c) 2008-2010 Bjoern Hoehrmann <bjoern@hoehrmann.de>
// See LICENSE.md file or http://bjoern.hoehrmann.de/utf-8/decoder/dfa/
// for more details.

#ifndef LUG_INCLUDE_LUG_UTF8_HPP
#define LUG_INCLUDE_LUG_UTF8_HPP

#include <lug/unicode.hpp>

namespace lug::utf8 {

// NOLINTBEGIN(cppcoreguidelines-avoid-magic-numbers,readability-magic-numbers)

namespace detail {

enum class decode_state : unsigned char { accept = 0, reject = 12 };

inline constexpr std::array<unsigned char, 256> dfa_class_table
{
	 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
	 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
	 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
	 8, 8, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
	 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
	10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 3, 3,
	11, 6, 6, 6, 5, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8
};

inline constexpr std::array<unsigned char, 108> dfa_transition_table
{
	0,12,24,36,60,96,84,12,12,12,48,72,12,12,12,12,
	12,12,12,12,12,12,12,12,12, 0,12,12,12,12,12, 0,
	12, 0,12,12,12,24,12,12,12,12,12,24,12,24,12,12,
	12,12,12,12,12,12,12,24,12,12,12,12,12,24,12,12,
	12,12,12,12,12,24,12,12,12,12,12,12,12,12,12,36,
	12,36,12,12,12,36,12,12,12,12,12,36,12,36,12,12,
	12,36,12,12,12,12,12,12,12,12,12,12
};

inline constexpr std::array<char, 3> utf8_replacement_sequence
{
	static_cast<char>(0xefU),
	static_cast<char>(0xbfU),
	static_cast<char>(0xbdU)
};

inline constexpr char32_t utf32_replacement = U'\U0000fffd';

[[nodiscard]] constexpr decode_state decode_rune_octet(char32_t& rune, char octet, decode_state state) noexcept
{
	auto const symbol = static_cast<unsigned int>(static_cast<unsigned char>(octet));
	auto const dfa_class = static_cast<unsigned int>(dfa_class_table[symbol]); // NOLINT(cppcoreguidelines-pro-bounds-constant-array-index)
	rune = (state == decode_state::accept) ? (symbol & (0xffU >> dfa_class)) : ((symbol & 0x3fU) | (rune << 6U));
	return static_cast<decode_state>(dfa_transition_table[static_cast<std::size_t>(state) + dfa_class]); // NOLINT(cppcoreguidelines-pro-bounds-constant-array-index)
}

[[nodiscard]] constexpr unsigned int non_ascii_rune_length(char32_t rune) noexcept
{
	if (rune < 0x00000800U)
		return 2;
	if (rune < 0x00010000U)
		return 3;
	return 4;
}

} // namespace detail

[[nodiscard]] constexpr bool is_lead(char octet) noexcept
{
	return (static_cast<unsigned char>(octet) & 0xc0U) != 0x80U;
}

template <class InputIt, class = lug::detail::enable_if_char_input_iterator_t<InputIt>>
[[nodiscard]] constexpr std::pair<InputIt, char32_t> decode_rune(InputIt first, InputIt last)
{
	char32_t rune = U'\0';
	detail::decode_state state = detail::decode_state::accept;
	while ((first != last) && (state != detail::decode_state::reject))
		if (state = utf8::detail::decode_rune_octet(rune, *first++, state); state == detail::decode_state::accept)
			return std::make_pair(first, rune);
	return std::make_pair(std::find_if(first, last, lug::utf8::is_lead), detail::utf32_replacement);
}

template <class InputIt, class = lug::detail::enable_if_char_input_iterator_t<InputIt>>
[[nodiscard]] constexpr InputIt next_rune(InputIt first, InputIt last)
{
	return lug::utf8::decode_rune(first, last).first;
}

template <class InputIt, class = lug::detail::enable_if_char_input_iterator_t<InputIt>>
[[nodiscard]] constexpr std::size_t count_runes(InputIt first, InputIt last)
{
	std::size_t count = 0;
	for (; first != last; ++count)
		first = lug::utf8::next_rune(first, last);
	return count;
}

template <class OutputIt>
inline std::pair<OutputIt, bool> encode_rune(OutputIt dst, char32_t rune)
{
	if (rune < 0x80) {
		*dst++ = static_cast<char>(rune);
	} else {
		if ((0x00110000U <= rune) || ((rune & 0xfffff800U) == 0x0000d800U))
			return {std::copy(detail::utf8_replacement_sequence.begin(), detail::utf8_replacement_sequence.end(), dst), false};
		unsigned int const n = detail::non_ascii_rune_length(rune);
		for (unsigned int i = 0, c = ((0xf0U << (4 - n)) & 0xf0U); i < n; ++i, c = 0x80U)
			*dst++ = static_cast<char>(((rune >> (6 * (n - i - 1))) & 0x3fU) | c);
	}
	return {dst, true};
}

// NOLINTEND(cppcoreguidelines-avoid-magic-numbers,readability-magic-numbers)

[[nodiscard]] inline std::string encode_rune(char32_t rune)
{
	std::string result;
	encode_rune(std::back_inserter(result), rune);
	return result;
}

inline constexpr struct
{
	template <class InputIt, class OutputIt>
	OutputIt operator()(InputIt first, InputIt last, OutputIt dst) const
	{
		while (first != last) {
			auto [next, rune] = lug::utf8::decode_rune(first, last);
			dst = lug::utf8::encode_rune(dst, unicode::tocasefold(rune)).first;
			first = next;
		}
		return dst;
	}

	[[nodiscard]] std::string operator()(std::string_view src) const
	{
		std::string result;
		result.reserve(src.size());
		(*this)(std::begin(src), std::end(src), std::back_inserter(result));
		return result;
	}
}
tocasefold{};

inline constexpr struct
{
	template <class InputIt, class OutputIt>
	OutputIt operator()(InputIt first, InputIt last, OutputIt dst) const
	{
		while (first != last) {
			auto [next, rune] = lug::utf8::decode_rune(first, last);
			dst = lug::utf8::encode_rune(dst, unicode::tolower(rune)).first;
			first = next;
		}
		return dst;
	}

	[[nodiscard]] std::string operator()(std::string_view src) const
	{
		std::string result;
		result.reserve(src.size());
		(*this)(std::begin(src), std::end(src), std::back_inserter(result));
		return result;
	}
}
tolower{};

inline constexpr struct
{
	template <class InputIt, class OutputIt>
	OutputIt operator()(InputIt first, InputIt last, OutputIt dst) const
	{
		while (first != last) {
			auto [next, rune] = lug::utf8::decode_rune(first, last);
			dst = lug::utf8::encode_rune(dst, unicode::toupper(rune)).first;
			first = next;
		}
		return dst;
	}

	[[nodiscard]] std::string operator()(std::string_view src) const
	{
		std::string result;
		result.reserve(src.size());
		(*this)(std::begin(src), std::end(src), std::back_inserter(result));
		return result;
	}
}
toupper{};

} // namespace lug::utf8

#endif
