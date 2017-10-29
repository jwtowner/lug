// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017 Jesse W. Towner
// See LICENSE.md file for license details

#ifndef LUG_DETAIL_HPP__
#define LUG_DETAIL_HPP__

#include <limits>
#include <type_traits>

namespace lug::detail
{

template <class T>
struct dynamic_cast_if_base_of
{
	std::remove_reference_t<T>& b;

	template <class U, class = std::enable_if_t<std::is_base_of_v<std::decay_t<T>, std::decay_t<U>>>>
	operator U&() const volatile {
		return dynamic_cast<std::remove_reference_t<U>&>(b);
	}
};

template <class Error>
struct reentrancy_sentinel
{
	bool& value;

	reentrancy_sentinel(bool& x) : value{x} {
		if (value)
			throw Error{};
		value = true;
	}

	~reentrancy_sentinel() {
		value = false;
	}
};

template <class Error, class T, class U, class V>
inline void assure_in_range(T x, U minval, V maxval)
{
	if (!(minval <= x && x <= maxval))
		throw Error{};
}

template <class Error, class T, class U>
inline auto checked_add(T x, U y)
{
	if ((std::numeric_limits<decltype(x + y)>::max)() - x < y)
		throw Error{};
	return x + y;
}

template <std::size_t... Indices, class Tuple>
constexpr auto make_tuple_view(Tuple&& t) noexcept
{
	return ::std::forward_as_tuple(::std::get<Indices>(::std::forward<Tuple>(t))...);
}

enum class search_status { accept, reject, escape };

template<class InputIt, class UnaryPredicate>
InputIt escaping_find_if(InputIt first, InputIt last, UnaryPredicate p)
{
	for ( ; first != last; ++first) {
		if (auto s = p(*first); s == search_status::accept)
			return first;
		else if (s == search_status::escape)
			break;
	}
	return last;
}

template <class Sequence>
inline auto pop_back(Sequence& s)
{
	typename Sequence::value_type result{::std::move(s.back())};
	s.pop_back();
	return result;
}

template <class Sequence, class T, class BinaryOp>
inline auto reduce_back(Sequence& s, T value, BinaryOp op)
{
	typename Sequence::value_type result{::std::move(s.back())};
	s.back() = op(result, value);
	return result;
}

} // namespace lug::detail

#endif
