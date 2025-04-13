// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#ifndef LUG_INCLUDE_LUG_RUNE_HPP
#define LUG_INCLUDE_LUG_RUNE_HPP

#include <lug/ascii.hpp>
#include <lug/utf8.hpp>

#include <bitset>
#include <memory>
#include <variant>
#include <vector>

namespace lug {

constexpr std::size_t ascii_bitset_size = 128;
using ascii_bitset = std::bitset<ascii_bitset_size>;

class rune_set;
class rune_set_builder;

class rune_set
{
	friend class rune_set_builder;

public:
	[[nodiscard]] static rune_set const& none();
	[[nodiscard]] static rune_set const& all();

	constexpr rune_set() noexcept = default;

	constexpr explicit rune_set(ascii_bitset const& aset) noexcept
		: ascii_set_{aset}
	{}

	explicit rune_set(char32_t rune)
		: ascii_set_{}
		, intervals_{}
		, intervals_size_{0}
	{
		if (ascii::isascii(rune)) {
			ascii_set_[static_cast<std::size_t>(static_cast<unsigned char>(rune))] = true;
		} else {
			auto intervals = detail::make_shared_array<std::pair<char32_t, char32_t>>(1);
			*intervals.get() = {rune, rune};
			intervals_ = std::move(intervals);
			intervals_size_ = 1;
		}
	}

	rune_set(rune_set const& other)
		: ascii_set_{other.ascii_set_}
		, intervals_{other.intervals_}
		, intervals_size_{other.intervals_size_}
	{}

	rune_set(rune_set&& other) noexcept
		: ascii_set_{other.ascii_set_}
		, intervals_{std::move(other.intervals_)}
		, intervals_size_{std::exchange(other.intervals_size_, 0)}
	{}

	rune_set& operator=(rune_set const& other)
	{
		rune_set{other}.swap(*this);
		return *this;
	}

	rune_set& operator=(rune_set&& other) noexcept
	{
		rune_set{std::move(other)}.swap(*this);
		return *this;
	}
	
	~rune_set() = default;

	void swap(rune_set& other) noexcept
	{
		std::swap(ascii_set_, other.ascii_set_);
		intervals_.swap(other.intervals_);
		std::swap(intervals_size_, other.intervals_size_);
	}

	friend void swap(rune_set& lhs, rune_set& rhs) noexcept
	{
		lhs.swap(rhs);
	}

	[[nodiscard]] bool operator==(rune_set const& rhs) const noexcept
	{
		return (ascii_set_ == rhs.ascii_set_) &&
				(intervals_size_ == rhs.intervals_size_) &&
				(intervals_size_ == 0 || std::equal(intervals_.get(), intervals_.get() + intervals_size_, rhs.intervals_.get()));
	}

	[[nodiscard]] bool operator!=(rune_set const& rhs) const noexcept
	{
		return !(*this == rhs);
	}

	template <class InputIt, class = std::enable_if_t<detail::is_char_input_iterator_v<InputIt>>>
	[[nodiscard]] LUG_ALWAYS_INLINE auto operator()(InputIt first, InputIt last) const -> std::optional<std::decay_t<InputIt>>
	{
		if LUG_LIKELY(first != last) {
			if (auto const c = *first++; ascii::isascii(c)) {
				if (ascii_set_[static_cast<std::size_t>(static_cast<unsigned char>(c))]) {
					return first;
				}
			} else if (intervals_size_ > 0) {
				auto const [next, rune] = utf8::decode_rune_rest(c, first, last);
				auto const interval = std::lower_bound(intervals_.get(), intervals_.get() + intervals_size_, rune, [](auto const& x, auto const& y) noexcept { return x.second < y; });
				if ((interval != intervals_.get() + intervals_size_) && (interval->first <= rune) && (rune <= interval->second)) {
					return next;
				}
			}
		}
		return std::nullopt;
	}

	template <class InputRng, class = std::enable_if_t<detail::is_char_input_range_v<InputRng>>>
	[[nodiscard]] LUG_ALWAYS_INLINE auto operator()(InputRng&& rng) const -> std::optional<std::decay_t<decltype(std::begin(rng))>> // NOLINT(cppcoreguidelines-missing-std-forward)
	{
		return (*this)(std::begin(rng), std::end(rng));
	}

	template <class InputIt, class = std::enable_if_t<detail::is_char_input_iterator_v<InputIt>>>
	[[nodiscard]] LUG_ALWAYS_INLINE auto match(InputIt first, InputIt last) const -> std::optional<std::decay_t<InputIt>>
	{
		return match(first, last);
	}

	template <class InputRng, class = std::enable_if_t<detail::is_char_input_range_v<InputRng>>>
	[[nodiscard]] LUG_ALWAYS_INLINE auto match(InputRng&& rng) const -> std::optional<std::decay_t<decltype(std::begin(rng))>> // NOLINT(cppcoreguidelines-missing-std-forward)
	{
		return match(std::begin(rng), std::end(rng));
	}

	[[nodiscard]] bool contains(char32_t rune) const noexcept
	{
		if (rune < ascii_limit)
			return ascii_set_[static_cast<std::size_t>(rune)];
		auto const interval = std::lower_bound(intervals_.get(), intervals_.get() + intervals_size_, rune, [](auto const& x, auto const& y) noexcept { return x.second < y; });
		return (interval != intervals_.get() + intervals_size_) && (interval->first <= rune) && (rune <= interval->second);
	}

	[[nodiscard]] bool empty() const noexcept
	{
		return ascii_set_.none() && (intervals_size_ == 0);
	}

	[[nodiscard]] bool full() const noexcept
	{
		return ascii_set_.all() && (intervals_size_ == 1) && (intervals_.get()->first == ascii_limit) && (intervals_.get()->second == rune_max);
	}

	[[nodiscard]] bool single() const noexcept
	{
		return ((ascii_set_.count() == 1) && (intervals_size_ == 0)) || ((ascii_set_.count() == 0) && (intervals_size_ == 1) && (intervals_.get()->first == intervals_.get()->second));
	}

	[[nodiscard]] std::optional<char32_t> as_rune() const noexcept
	{
		if ((ascii_set_.count() == 1) && (intervals_size_ == 0)) {
			for (std::size_t i = 0; i < ascii_set_.size(); ++i) {
				if (ascii_set_[i]) {
					return static_cast<char32_t>(i);
				}
			}
			return std::nullopt;
		}
		if ((ascii_set_.count() == 0) && (intervals_size_ == 1) && (intervals_.get()->first == intervals_.get()->second)) {
			return intervals_.get()->first;
		}
		return std::nullopt;
	}

	[[nodiscard]] bool disjoint(rune_set const& other) const noexcept
	{
		if ((ascii_set_ & other.ascii_set_).any()) {
			return false;
		}
		if (intervals_size_ == 0 || other.intervals_size_ == 0) {
			return true;
		}
		auto lcur = intervals_.get();
		auto const lend = lcur + intervals_size_;
		auto rcur = intervals_.get();
		auto const rend = rcur + other.intervals_size_;
		while ((lcur != lend) && (rcur != rend)) {
			if (lcur->first < rcur->first) {
				if (lcur->second >= rcur->first) {
					return false;
				}
				++lcur;
			} else if (rcur->first < lcur->first) {
				if (rcur->second >= lcur->first) {
					return false;
				}
				++rcur;
			} else {
				return false;
			}
		}
		return true;
	}

	[[nodiscard]] rune_set complement() const;
	[[nodiscard]] rune_set intersect_with(rune_set const& other) const;
	[[nodiscard]] rune_set union_with(rune_set const& other) const;

private:
	static constexpr char32_t ascii_limit = U'\U00000080';
	static constexpr char32_t unicode_limit = U'\U0010FFFF';
	static constexpr char32_t rune_max = (std::numeric_limits<char32_t>::max)();

	[[nodiscard]] static rune_set build_all();

	// NOLINTNEXTLINE(cppcoreguidelines-avoid-c-arrays,hicpp-avoid-c-arrays,modernize-avoid-c-arrays)
	rune_set(ascii_bitset const& aset, std::shared_ptr<std::pair<char32_t, char32_t> const>&& intervals, std::size_t size) noexcept
		: ascii_set_{aset}
		, intervals_{std::move(intervals)}
		, intervals_size_{size}
	{}

	ascii_bitset ascii_set_;
	std::shared_ptr<std::pair<char32_t, char32_t> const> intervals_; // NOLINT(cppcoreguidelines-avoid-c-arrays,hicpp-avoid-c-arrays,modernize-avoid-c-arrays)
	std::size_t intervals_size_{0};
};

class rune_set_builder
{
public:
	rune_set_builder& casefold(bool value = true)
	{
		casefolded_ = value;
		return *this;
	}

	rune_set_builder& negate(bool value = true)
	{
		negated_ = value;
		return *this;
	}

	rune_set_builder& add_rune(char32_t rune)
	{
		if LUG_UNLIKELY(rune >= rune_set::unicode_limit) {
			throw_exception<bad_character_range>();
		}
		if (casefolded_) {
			push_casefolded_rune(rune);
		} else {
			push_rune(rune);
		}
		return *this;
	}

	rune_set_builder& add_runes(std::initializer_list<char32_t> runes)
	{
		for (char32_t const r : runes) {
			add_rune(r);
		}
		return *this;
	}

	rune_set_builder& add_range(char32_t start, char32_t end)
	{
		if LUG_UNLIKELY((start > end) || (end >= rune_set::unicode_limit)) {
			throw_exception<bad_character_range>();
		}
		if (casefolded_) {
			push_casefolded_range(start, end);
		} else {
			push_range(start, end);
		}
		return *this;
	}

	rune_set_builder& add_range(std::pair<char32_t, char32_t> const& range)
	{
		return add_range(range.first, range.second);
	}

	rune_set_builder& add_ranges(std::initializer_list<std::pair<char32_t, char32_t>> ranges)
	{
		for (auto const& range : ranges)
			add_range(range);
		return *this;
	}

	rune_set_builder& add_rune_set(rune_set const& set)
	{
		if (casefolded_) {
			for (std::size_t i = 0; i < set.ascii_set_.size(); ++i) {
				if (set.ascii_set_[i]) {
					push_casefolded_rune(static_cast<char32_t>(i));
				}
			}
			std::for_each_n(set.intervals_.get(), set.intervals_size_, [this](auto const& r) { push_casefolded_range(r.first, r.second); });
		} else {
			ascii_set_ |= set.ascii_set_;
			std::for_each_n(set.intervals_.get(), set.intervals_size_, [this](auto const& r) { push_range(r.first, r.second); });
		}
		return *this;
	}

	[[nodiscard]] rune_set build() &&
	{
		std::vector<std::pair<char32_t, char32_t>> optimized;
		std::sort_heap(intervals_.begin(), intervals_.end());
		auto out = optimized.end();
		for (auto const& r : intervals_) {
			if (out == optimized.end() || r.first < out->first || out->second < r.first) {
				out = optimized.insert(optimized.end(), r);
			} else {
				out->second = out->second < r.second ? r.second : out->second;
			}
		}
		if (negated_) {
			return make_rune_set(~ascii_set_, negate_intervals(optimized));
		}
		return make_rune_set(ascii_set_, optimized);
	}

private:
	[[nodiscard]] static std::vector<std::pair<char32_t, char32_t>> negate_intervals(std::vector<std::pair<char32_t, char32_t>> const& intervals)
	{
		std::vector<std::pair<char32_t, char32_t>> result;
		if (!intervals.empty()) {
			if (char32_t const front = intervals.front().first; rune_set::ascii_limit < front) {
				result.emplace_back(rune_set::ascii_limit, front - 1);
			}
			if (intervals.size() > 1) {
				auto const last = intervals.cend();
				for (auto left = intervals.cbegin(), right = left + 1; right != last; ++left, ++right) {
					result.emplace_back(left->second + 1, right->first - 1);
				}
			}
			if (char32_t const back = intervals.back().second; back < rune_set::rune_max) {
				result.emplace_back(back + 1, rune_set::rune_max);
			}
		} else {
			result.emplace_back(rune_set::ascii_limit, rune_set::rune_max);
		}
		return result;
	}

	[[nodiscard]] static rune_set make_rune_set(ascii_bitset const& aset, std::vector<std::pair<char32_t, char32_t>> const& intervals)
	{
		if (intervals.empty())
			return rune_set{aset};
		auto interval_array = detail::make_shared_array<std::pair<char32_t, char32_t>>(intervals.size());
		std::copy(intervals.begin(), intervals.end(), interval_array.get());
		return rune_set{aset, std::move(interval_array), intervals.size()};
	}

	void push_rune(char32_t rune)
	{
		if (rune < rune_set::ascii_limit) {
			ascii_set_[static_cast<std::size_t>(rune)] = true;
		} else {
			intervals_.emplace_back(rune, rune);
			std::push_heap(intervals_.begin(), intervals_.end());
		}
	}

	void push_casefolded_rune(char32_t rune)
	{
		push_rune(unicode::tolower(rune));
		push_rune(unicode::toupper(rune));
		push_rune(unicode::tocasefold(rune));
	}

	void push_range(char32_t start, char32_t end)
	{
		for (char32_t rn = start; rn <= end && rn < rune_set::ascii_limit; ++rn) {
			ascii_set_[static_cast<std::size_t>(rn)] = true;
		}
		if (end >= rune_set::ascii_limit) {
			intervals_.emplace_back((std::max)(start, rune_set::ascii_limit), end);
			std::push_heap(intervals_.begin(), intervals_.end());
		}
	}

	void push_casefolded_range(char32_t start, char32_t end)
	{
		unicode::ptype p = unicode::query(start).properties();
		char32_t r1 = start;
		char32_t r2 = start;
		for (char32_t rn = start + 1; rn <= end; r2 = rn, ++rn) {
			unicode::ptype const q = unicode::query(rn).properties();
			if (((p ^ q) & unicode::ptype::Cased) != unicode::ptype::None) {
				push_uniform_casefolded_range(p, r1, r2);
				r1 = rn;
				p = q;
			}
		}
		push_uniform_casefolded_range(p, r1, r2);
	}

	void push_uniform_casefolded_range(unicode::ptype props, char32_t start, char32_t end)
	{
		if ((props & unicode::ptype::Cased) != unicode::ptype::None) {
			push_range(unicode::tolower(start), unicode::tolower(end));
			push_range(unicode::toupper(start), unicode::toupper(end));
			push_range(unicode::tocasefold(start), unicode::tocasefold(end));
		} else {
			push_range(start, end);
		}
	}

	ascii_bitset ascii_set_;
	std::vector<std::pair<char32_t, char32_t>> intervals_;
	bool casefolded_{false};
	bool negated_{false};
};

[[nodiscard]] inline rune_set const& rune_set::none()
{
	static rune_set const none_{};
	return none_;
}

[[nodiscard]] inline rune_set const& rune_set::all()
{
	static rune_set const all_{build_all()};
	return all_;
}

[[nodiscard]] inline rune_set rune_set::build_all()
{
	return std::move(rune_set_builder{}.negate()).build();
}

[[nodiscard]] inline rune_set rune_set::complement() const
{
	return std::move(rune_set_builder{}.negate().add_rune_set(*this)).build();
}

[[nodiscard]] inline rune_set rune_set::intersect_with(rune_set const& other) const
{
	if ((this == &other) || other.full()) {
		return *this;
	}
	if (full()) {
		return other;
	}
	if (empty() || other.empty()) {
		return none();
	}
	return std::move(rune_set_builder{}.negate().add_rune_set(complement()).add_rune_set(other.complement())).build();
}

[[nodiscard]] inline rune_set rune_set::union_with(rune_set const& other) const
{
	if ((this == &other) || other.empty()) {
		return *this;
	}
	if (empty()) {
		return other;
	}
	if (full() || other.full()) {
		return all();
	}
	return std::move(rune_set_builder{}.add_rune_set(*this).add_rune_set(other)).build();
}

namespace detail {

[[nodiscard]] inline rune_set build_unicode_space_set()
{
	rune_set_builder builder;
	builder.add_runes({U'\u0020', U'\u0085', U'\u00A0', U'\u1680', U'\u2028', U'\u2029', U'\u202F', U'\u205F', U'\u3000'}).add_range(U'\u0009', U'\u000D').add_range(U'\u2000', U'\u200A');
	return std::move(builder).build();
}

[[nodiscard]] inline rune_set build_unicode_blank_set()
{
	rune_set_builder builder;
	builder.add_runes({U'\u0009', U'\u0020', U'\u00A0', U'\u1680', U'\u202F', U'\u205F', U'\u3000'}).add_range(U'\u2000', U'\u200A');
	return std::move(builder).build();
}

[[nodiscard]] inline rune_set build_unicode_eol_set()
{
	rune_set_builder builder;
	builder.add_runes({U'\u0085', U'\u2028', U'\u2029'}).add_range(U'\u000A', U'\u000D');
	return std::move(builder).build();
}

} // namespace detail

namespace ascii {

[[nodiscard]] inline rune_set ctype_rune_set(ctype properties) noexcept
{
	ascii_bitset result;
	for (std::size_t i = 0; i < result.size(); ++i) {
		if ((ascii_ctype_table[i] & properties) != ctype::none) {
			result[i] = true;
		}
	}
	return rune_set{result};
}

[[nodiscard]] inline rune_set const& space_rune_set() noexcept
{
	static rune_set const space_{ascii_bitset{0x0000'0000'0000'3e00ULL}};
	return space_;
}

[[nodiscard]] inline rune_set const& blank_rune_set() noexcept
{
	static rune_set const blank_{ascii_bitset{0x0000'0000'0000'0200ULL}};
	return blank_;
}

[[nodiscard]] inline rune_set const& eol_rune_set() noexcept
{
	static rune_set const eol_{ascii_bitset{0x0000'0000'0000'3c00ULL}};
	return eol_;
}

} // namespace ascii

namespace unicode {

[[nodiscard]] inline rune_set const& space_rune_set()
{
	static rune_set const space_{lug::detail::build_unicode_space_set()};
	return space_;
}

[[nodiscard]] inline rune_set const& blank_rune_set()
{
	static rune_set const blank_{lug::detail::build_unicode_blank_set()};
	return blank_;
}

[[nodiscard]] inline rune_set const& eol_rune_set()
{
	static rune_set const eol_{lug::detail::build_unicode_eol_set()};
	return eol_;
}

} // namespace unicode

} // namespace lug

#endif
