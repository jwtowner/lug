// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017 Jesse W. Towner
// See LICENSE.md file for license details

#ifndef LUG_DETAIL_HPP__
#define LUG_DETAIL_HPP__

#include <algorithm>
#include <functional>
#include <iterator>
#include <limits>
#include <string>
#include <string_view>
#include <type_traits>

#ifdef __GNUC__

#define LUG_DIAGNOSTIC_PUSH_AND_IGNORE \
_Pragma("GCC diagnostic push") \
_Pragma("GCC diagnostic ignored \"-Wparentheses\"") \
_Pragma("GCC diagnostic ignored \"-Wlogical-not-parentheses\"")
_Pragma("GCC diagnostic ignored \"-Wunused-but-set-variable\"")

#define LUG_DIAGNOSTIC_POP \
_Pragma("GCC diagnostic pop")

#else

#define LUG_DIAGNOSTIC_PUSH_AND_IGNORE
#define LUG_DIAGNOSTIC_POP

#endif

namespace lug
{

inline namespace bitfield_ops
{

template <class T, class = std::void_t<decltype(T::is_bitfield_enum)>>
constexpr T operator~(T x) noexcept
{
	return static_cast<T>(~static_cast<std::underlying_type_t<T>>(x));
}

template <class T, class = std::void_t<decltype(T::is_bitfield_enum)>>
constexpr T operator&(T x, T y) noexcept
{
	return static_cast<T>(static_cast<std::underlying_type_t<T>>(x) & static_cast<std::underlying_type_t<T>>(y));
}

template <class T, class = std::void_t<decltype(T::is_bitfield_enum)>>
constexpr T operator|(T x, T y) noexcept
{
	return static_cast<T>(static_cast<std::underlying_type_t<T>>(x) | static_cast<std::underlying_type_t<T>>(y));
}

template <class T, class = std::void_t<decltype(T::is_bitfield_enum)>>
constexpr T operator^(T x, T y) noexcept
{
	return static_cast<T>(static_cast<std::underlying_type_t<T>>(x) ^ static_cast<std::underlying_type_t<T>>(y));
}

template <class T, class = std::void_t<decltype(T::is_bitfield_enum)>>
constexpr T& operator&=(T& x, T y) noexcept
{
	return (x = x & y);
}

template <class T, class = std::void_t<decltype(T::is_bitfield_enum)>>
constexpr T& operator|=(T& x, T y) noexcept
{
	return (x = x | y);
}

template <class T, class = std::void_t<decltype(T::is_bitfield_enum)>>
constexpr T& operator^=(T& x, T y) noexcept
{
	return (x = x ^ y);
}

} // namespace bitfield_ops

namespace detail
{

template <class... Args>
constexpr void ignore(Args&&...) noexcept {}

template <class T> struct member_pointer_object {};
template <class T, class U> struct member_pointer_object<T U::*> { typedef U type; };
template <class T> struct member_pointer_value {};
template <class T, class U> struct member_pointer_value<T U::*> { typedef T type; };

template <class ObjectIterator, class MemberPtrType, MemberPtrType MemberPtr>
class member_access_iterator
{
public:
	typedef ObjectIterator base_type;
	typedef typename member_pointer_object<MemberPtrType>::type object_type;
	typedef typename member_pointer_value<MemberPtrType>::type value_type;
	typedef value_type& reference;
	typedef value_type* pointer;
	typedef typename std::iterator_traits<base_type>::difference_type difference_type;
	typedef typename std::iterator_traits<base_type>::iterator_category iterator_category;
	constexpr member_access_iterator() noexcept : object_{} {};
	constexpr explicit member_access_iterator(ObjectIterator obj) : object_{obj} {}
	constexpr base_type base() const { return object_; }
	constexpr pointer operator->() const { return (*object_).*MemberPtr; }
	constexpr reference operator*() const { return (*object_).*MemberPtr; }
	constexpr reference operator[](difference_type n) const { return (object_[n]).*MemberPtr; }
	member_access_iterator& operator++() { ++object_; return *this; }
	member_access_iterator& operator--() { --object_; return *this; }
	member_access_iterator operator++(int) { member_access_iterator i{object_}; ++object_; return i; }
	member_access_iterator operator--(int) { member_access_iterator i{object_}; --object_; return i; }
	member_access_iterator& operator+=(difference_type n) { object_ += n; return *this; }
	member_access_iterator& operator-=(difference_type n) { object_ -= n; return *this; }
	friend constexpr bool operator==(member_access_iterator const& x, member_access_iterator const& y) noexcept { return x.object_ == y.object_; }
	friend constexpr bool operator!=(member_access_iterator const& x, member_access_iterator const& y) noexcept { return x.object_ != y.object_; }
	friend constexpr bool operator<(member_access_iterator const& x, member_access_iterator const& y) noexcept { return x.object_ < y.object_; }
	friend constexpr bool operator<=(member_access_iterator const& x, member_access_iterator const& y) noexcept { return x.object_ <= y.object_; }
	friend constexpr bool operator>(member_access_iterator const& x, member_access_iterator const& y) noexcept { return x.object_ > y.object_; }
	friend constexpr bool operator>=(member_access_iterator const& x, member_access_iterator const& y) noexcept { return x.object_ >= y.object_; }
	friend constexpr member_access_iterator operator+(member_access_iterator const& x, difference_type n) noexcept { return member_access_iterator{x.object_ + n}; }
	friend constexpr member_access_iterator operator+(difference_type n, member_access_iterator const& x) noexcept { return member_access_iterator{x.object_ + n}; }
	friend constexpr member_access_iterator operator-(member_access_iterator const& x, difference_type n) noexcept { return member_access_iterator{x.object_ - n}; }
	friend constexpr difference_type operator-(member_access_iterator const& x, member_access_iterator const& y) noexcept { return x.object_ - y.object_; }
private:
	ObjectIterator object_;
};

template <class MemberPtrType, MemberPtrType MemberPtr, class ObjectIterator>
constexpr auto make_member_accessor(ObjectIterator b)
{
	return member_access_iterator<ObjectIterator, MemberPtrType, MemberPtr>{b};
}

template <class T>
struct dynamic_cast_if_base_of
{
	std::remove_reference_t<T>& value;

	template <class U, class = std::enable_if_t<std::is_base_of_v<std::decay_t<T>, std::decay_t<U>>>>
	operator U&() const volatile {
		return dynamic_cast<std::remove_reference_t<U>&>(value);
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

template<class InputIt, class UnaryPredicate>
inline InputIt escaping_find_if(InputIt first, InputIt last, UnaryPredicate p)
{
	for ( ; first != last; ++first) {
		if (int status = p(*first); status > 0)
			return first;
		else if (status < 0)
			break;
	}
	return last;
}

template <class Sequence, class T>
inline std::size_t push_back_unique(Sequence& s, T&& x)
{
	if (auto i = ::std::find(::std::cbegin(s), ::std::cend(s), x); i != ::std::cend(s))
		return static_cast<std::size_t>(::std::distance(::std::cbegin(s), i));
	s.push_back(::std::forward<T>(x));
	return s.size() - 1;
}

template <class Sequence>
inline auto pop_back(Sequence& s)
{
	typename Sequence::value_type result{::std::move(s.back())};
	s.pop_back();
	return result;
}

template <class Integral>
inline std::string string_pack(Integral n) noexcept
{
	return std::string{reinterpret_cast<char const*>(&n), sizeof(n)};
}

template <class Integral>
inline Integral string_unpack(std::string_view s) noexcept
{
	return *reinterpret_cast<Integral const*>(s.data());
}

} // namespace detail

} // namespace lug

#endif
