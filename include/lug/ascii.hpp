// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#ifndef LUG_INCLUDE_LUG_ASCII_HPP
#define LUG_INCLUDE_LUG_ASCII_HPP

#include <lug/detail.hpp>
#include <optional>

namespace lug::ascii {

struct ascii_match_space_fn
{
	template <class InputIt, class = std::enable_if_t<lug::detail::is_char_input_iterator_v<InputIt>>>
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(InputIt first, InputIt last) const -> std::optional<std::decay_t<InputIt>>
	{
		if LUG_LIKELY(first != last) {
			char const c = *first;
			if ((c == ' ') || (('\t' <= c) && (c <= '\r'))) {
				++first;
				return first;
			}
		}
		return std::nullopt;
	}

	template <class InputRng, class = std::enable_if_t<lug::detail::is_char_input_range_v<InputRng>>>
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(InputRng&& rng) const -> std::optional<std::decay_t<decltype(std::begin(rng))>> // NOLINT(cppcoreguidelines-missing-std-forward)
	{
		return (*this)(std::begin(rng), std::end(rng));
	}
};

inline constexpr ascii_match_space_fn match_space{};

struct ascii_match_blank_fn
{
	template <class InputIt, class = std::enable_if_t<lug::detail::is_char_input_iterator_v<InputIt>>>
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(InputIt first, InputIt last) const -> std::optional<std::decay_t<InputIt>>
	{
		if LUG_LIKELY(first != last) {
			char const c = *first;
			if ((c == ' ') || (c == '\t')) {
				++first;
				return first;
			}
		}
		return std::nullopt;
	}

	template <class InputRng, class = std::enable_if_t<lug::detail::is_char_input_range_v<InputRng>>>
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(InputRng&& rng) const -> std::optional<std::decay_t<decltype(std::begin(rng))>> // NOLINT(cppcoreguidelines-missing-std-forward)
	{
		return (*this)(std::begin(rng), std::end(rng));
	}
};

inline constexpr ascii_match_blank_fn match_blank{};

struct ascii_match_eol_fn
{
	template <class InputIt, class = std::enable_if_t<lug::detail::is_char_input_iterator_v<InputIt>>>
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(InputIt first, InputIt last) const -> std::optional<std::decay_t<InputIt>>
	{
		if LUG_LIKELY(first != last) {
			char const c = *first;
			if (('\n' <= c) && (c <= '\f')) {
				++first;
				return first;
			}
			if (c == '\r') {
				++first;
				if ((first != last) && (*first == '\n'))
					++first;
				return first;
			}
		}
		return std::nullopt;
	}

	template <class InputRng, class = std::enable_if_t<lug::detail::is_char_input_range_v<InputRng>>>
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(InputRng&& rng) const -> std::optional<std::decay_t<decltype(std::begin(rng))>> // NOLINT(cppcoreguidelines-missing-std-forward)
	{
		return (*this)(std::begin(rng), std::end(rng));
	}
};

inline constexpr ascii_match_eol_fn match_eol{};

} // namespace lug::ascii

#endif
