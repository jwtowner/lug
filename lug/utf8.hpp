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

namespace detail {

inline constexpr unsigned int decode_accept = 0;
inline constexpr unsigned int decode_reject = 12;

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

} // namespace detail

[[nodiscard]] constexpr bool is_lead(char octet) noexcept
{
	return (static_cast<unsigned char>(octet) & 0xc0) != 0x80;
}

[[nodiscard]] constexpr unsigned int decode_rune_octet(char32_t& rune, char octet, unsigned int state)
{
	unsigned int const symbol = static_cast<unsigned int>(static_cast<unsigned char>(octet));
	unsigned int const dfa_class = static_cast<unsigned int>(detail::dfa_class_table[symbol]);
	rune = state == detail::decode_accept ? (symbol & (0xffU >> dfa_class)) : ((symbol & 0x3fU) | (rune << 6));
	return detail::dfa_transition_table[state + dfa_class];
}

template <class InputIt, class = lug::detail::enable_if_char_input_iterator_t<InputIt>>
[[nodiscard]] constexpr std::pair<InputIt, char32_t> decode_rune(InputIt first, InputIt last)
{
	char32_t rune = U'\0';
	unsigned int state = detail::decode_accept;
	while (first != last && state != detail::decode_reject)
		if (state = lug::utf8::decode_rune_octet(rune, *first++, state); state == detail::decode_accept)
			return std::make_pair(first, rune);
	return std::make_pair(std::find_if(first, last, lug::utf8::is_lead), U'\U0000fffd');
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
		if (0x00110000U <= rune || (rune & 0xfffff800U) == 0x0000d800U)
			return {std::copy_n(reinterpret_cast<char const*>(u8"\U0000fffd"), 3, dst), false};
		unsigned int const n = rune >= 0x00010000U ? 4 : rune >= 0x00000800U ? 3 : 2;
		for (unsigned int i = 0, c = (0xf0 << (4 - n)) & 0xf0; i < n; ++i, c = 0x80)
			*dst++ = static_cast<char>(((rune >> (6 * (n - i - 1))) & 0x3f) | c);
	}
	return {dst, true};
}

[[nodiscard]] inline std::string encode_rune(char32_t rune)
{
	std::string result;
	encode_rune(std::back_inserter(result), rune);
	return result;
}

inline constexpr struct
{
	template <class InputIt, class OutputIt>
	inline OutputIt operator()(InputIt first, InputIt last, OutputIt dst) const
	{
		while (first != last) {
			auto [next, rune] = lug::utf8::decode_rune(first, last);
			dst = lug::utf8::encode_rune(dst, unicode::tocasefold(rune)).first;
			first = next;
		}
		return dst;
	}

	[[nodiscard]] inline std::string operator()(std::string_view src) const
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
	inline OutputIt operator()(InputIt first, InputIt last, OutputIt dst) const
	{
		while (first != last) {
			auto [next, rune] = lug::utf8::decode_rune(first, last);
			dst = lug::utf8::encode_rune(dst, unicode::tolower(rune)).first;
			first = next;
		}
		return dst;
	}

	[[nodiscard]] inline std::string operator()(std::string_view src) const
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
	inline OutputIt operator()(InputIt first, InputIt last, OutputIt dst) const
	{
		while (first != last) {
			auto [next, rune] = lug::utf8::decode_rune(first, last);
			dst = lug::utf8::encode_rune(dst, unicode::toupper(rune)).first;
			first = next;
		}
		return dst;
	}

	[[nodiscard]] inline std::string operator()(std::string_view src) const
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
