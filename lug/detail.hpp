// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2024 Jesse W. Towner
// See LICENSE.md file for license details

#ifndef LUG_INCLUDE_LUG_DETAIL_HPP
#define LUG_INCLUDE_LUG_DETAIL_HPP

#include <algorithm>
#include <functional>
#include <iterator>
#include <limits>
#include <memory>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <vector>

#ifndef LUG_NO_RTTI
#if defined __GNUC__
#ifndef __GXX_RTTI
#define LUG_NO_RTTI
#endif
#elif defined _MSC_VER
#ifndef _CPPRTTI
#define LUG_NO_RTTI
#endif
#endif
#endif // LUG_NO_RTTI

#ifdef __GNUC__

#define LUG_DIAGNOSTIC_PUSH_AND_IGNORE \
_Pragma("GCC diagnostic push") \
_Pragma("GCC diagnostic ignored \"-Wparentheses\"") \
_Pragma("GCC diagnostic ignored \"-Wlogical-not-parentheses\"") \
_Pragma("GCC diagnostic ignored \"-Wuninitialized\"") \
_Pragma("GCC diagnostic ignored \"-Wunused-variable\"")
_Pragma("GCC diagnostic ignored \"-Wunused-but-set-variable\"")

#define LUG_DIAGNOSTIC_POP \
_Pragma("GCC diagnostic pop")

#else

#define LUG_DIAGNOSTIC_PUSH_AND_IGNORE
#define LUG_DIAGNOSTIC_POP

#endif

namespace lug {

inline namespace bitfield_ops {

template <class T, class = std::void_t<decltype(T::is_bitfield_enum)>>
[[nodiscard]] constexpr T operator~(T x) noexcept
{
	return static_cast<T>(~static_cast<std::underlying_type_t<T>>(x)); // NOLINT(clang-analyzer-optin.core.EnumCastOutOfRange)
}

template <class T, class = std::void_t<decltype(T::is_bitfield_enum)>>
[[nodiscard]] constexpr T operator&(T x, T y) noexcept
{
	return static_cast<T>(static_cast<std::underlying_type_t<T>>(x) & static_cast<std::underlying_type_t<T>>(y)); // NOLINT(clang-analyzer-optin.core.EnumCastOutOfRange)
}

template <class T, class = std::void_t<decltype(T::is_bitfield_enum)>>
[[nodiscard]] constexpr T operator|(T x, T y) noexcept
{
	return static_cast<T>(static_cast<std::underlying_type_t<T>>(x) | static_cast<std::underlying_type_t<T>>(y)); // NOLINT(clang-analyzer-optin.core.EnumCastOutOfRange)
}

template <class T, class = std::void_t<decltype(T::is_bitfield_enum)>>
[[nodiscard]] constexpr T operator^(T x, T y) noexcept
{
	return static_cast<T>(static_cast<std::underlying_type_t<T>>(x) ^ static_cast<std::underlying_type_t<T>>(y)); // NOLINT(clang-analyzer-optin.core.EnumCastOutOfRange)
}

template <class T, class = std::void_t<decltype(T::is_bitfield_enum)>>
constexpr T& operator&=(T& x, T y) noexcept
{
	return (x = x & y); // NOLINT(clang-analyzer-optin.core.EnumCastOutOfRange)
}

template <class T, class = std::void_t<decltype(T::is_bitfield_enum)>>
constexpr T& operator|=(T& x, T y) noexcept
{
	return (x = x | y); // NOLINT(clang-analyzer-optin.core.EnumCastOutOfRange)
}

template <class T, class = std::void_t<decltype(T::is_bitfield_enum)>>
constexpr T& operator^=(T& x, T y) noexcept
{
	return (x = x ^ y); // NOLINT(clang-analyzer-optin.core.EnumCastOutOfRange)
}

} // namespace bitfield_ops

namespace detail {

template <class T> inline constexpr bool always_false_v = false;

template <class V, class R, class Fn, class... Args> struct is_invocable_r_exact_impl : std::false_type {};
template <class R, class Fn, class... Args> struct is_invocable_r_exact_impl<std::void_t<std::enable_if_t<std::is_same_v<R, std::invoke_result_t<Fn, Args...>>>>, R, Fn, Args...> : std::true_type {};
template <class R, class Fn, class... Args> struct is_invocable_r_exact : is_invocable_r_exact_impl<void, R, Fn, Args...> {};
template <class R, class Fn, class... Args> inline constexpr bool is_invocable_r_exact_v = is_invocable_r_exact<R, Fn, Args...>::value;

template <class T> struct remove_cvref_from_tuple;
template <class T> struct remove_cvref_from_tuple<T const> : remove_cvref_from_tuple<T> {};
template <class T> struct remove_cvref_from_tuple<T volatile> : remove_cvref_from_tuple<T> {};
template <class T> struct remove_cvref_from_tuple<T const volatile> : remove_cvref_from_tuple<T> {};
template <class... Args> struct remove_cvref_from_tuple<std::tuple<Args...>> { using type = std::tuple<std::remove_cv_t<std::remove_reference_t<Args>>...>; };
template <class T> using remove_cvref_from_tuple_t = typename remove_cvref_from_tuple<T>::type;

template <class It>
inline constexpr bool is_char_contiguous_iterator_v =
	std::is_convertible_v<It, std::vector<char>::const_iterator> ||
	std::is_convertible_v<It, std::string::const_iterator> ||
	std::is_convertible_v<It, std::string_view::const_iterator> ||
	std::is_same_v<std::decay_t<It>, char*> ||
	std::is_same_v<std::decay_t<It>, const char*>;

template <class It, class T = void>
using enable_if_char_input_iterator_t = std::enable_if_t<
	!std::is_integral_v<It> &&
	std::is_base_of_v<std::input_iterator_tag, typename std::iterator_traits<It>::iterator_category> &&
	std::is_same_v<char, std::remove_cv_t<typename std::iterator_traits<It>::value_type>>, T>;

template <class It, class T = void>
using enable_if_char_contiguous_iterator_t = std::enable_if_t<is_char_contiguous_iterator_v<It>, T>;

template <class... Args>
constexpr void ignore(Args&&...) noexcept {} // NOLINT(cppcoreguidelines-missing-std-forward,hicpp-named-parameter,readability-named-parameter)

struct identity
{
	template <class T>
	[[nodiscard]] constexpr T&& operator()( T&& t ) const noexcept
	{
		return std::forward<T>(t);
	}
};

template <class T> struct member_pointer_object {};
template <class T, class U> struct member_pointer_object<T U::*> { using type = U; };
template <class T> struct member_pointer_value {};
template <class T, class U> struct member_pointer_value<T U::*> { using type = T; };

template <class ObjectIterator, class MemberPtrType, MemberPtrType MemberPtr>
class member_access_iterator
{
public:
	using base_type = ObjectIterator;
	using object_type = typename member_pointer_object<MemberPtrType>::type;
	using value_type = typename member_pointer_value<MemberPtrType>::type;
	using reference = value_type&;
	using pointer = value_type*;
	using difference_type = typename std::iterator_traits<base_type>::difference_type;
	using iterator_category = typename std::iterator_traits<base_type>::iterator_category;
	constexpr member_access_iterator() noexcept : object_{} {}
	constexpr explicit member_access_iterator(ObjectIterator obj) : object_{obj} {}
	[[nodiscard]] constexpr base_type base() const { return object_; }
	[[nodiscard]] constexpr pointer operator->() const { return (*object_).*MemberPtr; }
	[[nodiscard]] constexpr reference operator*() const { return (*object_).*MemberPtr; }
	[[nodiscard]] constexpr reference operator[](difference_type n) const { return (object_[n]).*MemberPtr; }
	constexpr member_access_iterator& operator++() { ++object_; return *this; }
	constexpr member_access_iterator& operator--() { --object_; return *this; }
	constexpr member_access_iterator operator++(int) { member_access_iterator const i{object_}; ++object_; return i; }
	constexpr member_access_iterator operator--(int) { member_access_iterator const i{object_}; --object_; return i; }
	constexpr member_access_iterator& operator+=(difference_type n) { object_ += n; return *this; }
	constexpr member_access_iterator& operator-=(difference_type n) { object_ -= n; return *this; }
	[[nodiscard]] friend constexpr bool operator==(member_access_iterator const& x, member_access_iterator const& y) noexcept { return x.object_ == y.object_; }
	[[nodiscard]] friend constexpr bool operator!=(member_access_iterator const& x, member_access_iterator const& y) noexcept { return x.object_ != y.object_; }
	[[nodiscard]] friend constexpr bool operator<(member_access_iterator const& x, member_access_iterator const& y) noexcept { return x.object_ < y.object_; }
	[[nodiscard]] friend constexpr bool operator<=(member_access_iterator const& x, member_access_iterator const& y) noexcept { return x.object_ <= y.object_; }
	[[nodiscard]] friend constexpr bool operator>(member_access_iterator const& x, member_access_iterator const& y) noexcept { return x.object_ > y.object_; }
	[[nodiscard]] friend constexpr bool operator>=(member_access_iterator const& x, member_access_iterator const& y) noexcept { return x.object_ >= y.object_; }
	[[nodiscard]] friend constexpr member_access_iterator operator+(member_access_iterator const& x, difference_type n) noexcept { return member_access_iterator{x.object_ + n}; }
	[[nodiscard]] friend constexpr member_access_iterator operator+(difference_type n, member_access_iterator const& x) noexcept { return member_access_iterator{x.object_ + n}; }
	[[nodiscard]] friend constexpr member_access_iterator operator-(member_access_iterator const& x, difference_type n) noexcept { return member_access_iterator{x.object_ - n}; }
	[[nodiscard]] friend constexpr difference_type operator-(member_access_iterator const& x, member_access_iterator const& y) noexcept { return x.object_ - y.object_; }
private:
	ObjectIterator object_;
};

template <class MemberPtrType, MemberPtrType MemberPtr, class ObjectIterator>
[[nodiscard]] constexpr auto make_member_accessor(ObjectIterator b)
{
	return member_access_iterator<ObjectIterator, MemberPtrType, MemberPtr>{b};
}

template <class T>
class dynamic_cast_if_base_of
{
	std::remove_reference_t<T>& value_; // NOLINT(cppcoreguidelines-avoid-const-or-ref-data-members)

public:
	constexpr explicit dynamic_cast_if_base_of(std::remove_reference_t<T>& x) noexcept : value_{x} {}

	template <class U, class = std::enable_if_t<std::is_base_of_v<std::decay_t<T>, std::decay_t<U>>>>
	[[nodiscard]] constexpr operator U&() const // NOLINT(hicpp-explicit-conversions)
	{
#ifndef LUG_NO_RTTI
		if constexpr (std::is_same_v<std::decay_t<T>, std::decay_t<U>>)
#endif // LUG_NO_RTTI
			return static_cast<std::remove_reference_t<U>&>(value_);
#ifndef LUG_NO_RTTI
		else
			return dynamic_cast<std::remove_reference_t<U>&>(value_);
#endif // LUG_NO_RTTI
	}
};

template <class Error>
class reentrancy_sentinel
{
	bool& value; // NOLINT(cppcoreguidelines-avoid-const-or-ref-data-members)

public:
	constexpr explicit reentrancy_sentinel(bool& x)
		: value{x}
	{
		if (value)
			throw Error();
		value = true;
	}

	~reentrancy_sentinel()
	{
		value = false;
	}

	reentrancy_sentinel(reentrancy_sentinel const&) = delete;
	reentrancy_sentinel(reentrancy_sentinel&&) = delete;
	reentrancy_sentinel& operator=(reentrancy_sentinel const&) = delete;
	reentrancy_sentinel& operator=(reentrancy_sentinel&&) = delete;
};

template <class EF>
class scope_exit
{
	static_assert(std::is_invocable_v<EF>);

	EF destructor;

public:
	template <class Fn, class = std::enable_if_t<std::is_constructible_v<EF, Fn&&>>>
	constexpr explicit scope_exit( Fn&& fn ) noexcept(std::is_nothrow_constructible_v<EF, Fn&&>)
		: destructor{std::forward<Fn>(fn)}
	{}

	~scope_exit()
	{
		destructor();
	}

	scope_exit(scope_exit const&) = delete;
	scope_exit(scope_exit&&) = delete;
	scope_exit& operator=(scope_exit const&) = delete;
	scope_exit& operator=(scope_exit&&) = delete;
};

template <class Fn, class = std::enable_if_t<std::is_invocable_v<Fn>>>
scope_exit(Fn) -> scope_exit<std::decay_t<Fn>>;

template <class Error, class T, class U, class V>
inline void assure_in_range(T x, U minval, V maxval)
{
	if (!((minval <= x) && (x <= maxval)))
		throw Error();
}

template <class Error, class T, class U>
[[nodiscard]] inline auto checked_add(T x, U y)
{
	if (((std::numeric_limits<decltype(x + y)>::max)() - x) < y)
		throw Error();
	return x + y;
}

template <std::size_t... Indices, class Tuple>
[[nodiscard]] constexpr auto make_tuple_view(Tuple&& t) noexcept
{
	return std::forward_as_tuple(std::get<Indices>(std::forward<Tuple>(t))...);
}

template<class InputIt, class UnaryPredicate>
[[nodiscard]] inline InputIt escaping_find_if(InputIt first, InputIt last, UnaryPredicate pred)
{
	for ( ; first != last; ++first) {
		const int status = pred(*first);
		if (status > 0)
			return first;
		if (status < 0)
			break;
	}
	return last;
}

template <class Sequence, class T>
inline std::size_t push_back_unique(Sequence& s, T&& x)
{
	if (auto i = std::find(std::cbegin(s), std::cend(s), x); i != std::cend(s))
		return static_cast<std::size_t>(std::distance(std::cbegin(s), i));
	s.push_back(std::forward<T>(x));
	return s.size() - 1;
}

template <class Sequence>
[[nodiscard]] inline auto pop_back(Sequence& s) -> typename Sequence::value_type
{
	typename Sequence::value_type result{std::move(s.back())};
	s.pop_back();
	return result;
}

template <class Integral>
[[nodiscard]] inline std::string string_pack(Integral n)
{
	return std::string{reinterpret_cast<char const*>(&n), sizeof(n)}; // NOLINT(cppcoreguidelines-pro-type-reinterpret-cast)
}

template <class Integral>
[[nodiscard]] inline Integral string_unpack(std::string_view s)
{
	return *reinterpret_cast<Integral const*>(s.data()); // NOLINT(cppcoreguidelines-pro-type-reinterpret-cast)
}

class frame_allocator
{
	static inline constexpr std::size_t large_object_size = 4096;
	static inline constexpr std::size_t frame_block_data_size = 65536;
	struct alignas(large_object_size) frame_block_data { std::byte bytes[frame_block_data_size]; };

	struct frame_block
	{
		std::unique_ptr<frame_block_data> data{std::make_unique<frame_block_data>()};
		std::size_t offset{0};
	};

	std::vector<frame_block> frame_blocks_{1};
	std::vector<frame_block>::iterator current_frame_block_{frame_blocks_.begin()};

public:
	void* allocate(std::size_t size, std::size_t alignment)
	{
		alignment = (alignment < alignof(std::size_t)) ? alignof(std::size_t) : alignment;
		std::vector<frame_block>::iterator block = current_frame_block_;
		std::size_t prev_offset = current_frame_block_->offset;
		std::size_t start_offset = (prev_offset + sizeof(std::size_t) + (alignment - 1)) & ~(alignment - 1);
		std::size_t end_offset = (start_offset + size + (alignof(std::size_t) - 1)) & ~(alignof(std::size_t) - 1);
		if (end_offset > frame_block_data_size) {
			if (++block == frame_blocks_.end())
				block = current_frame_block_ = frame_blocks_.insert(block, frame_block{});
			prev_offset = 0;
			start_offset = (sizeof(std::size_t) + (alignment - 1)) & ~(alignment - 1);
			end_offset = start_offset + size;
		}
		std::byte* const ptr = &block->data->bytes[start_offset];
		block->offset = end_offset;
		*reinterpret_cast<std::size_t*>(ptr - sizeof(std::size_t)) = prev_offset;
		return ptr;
	}

	void deallocate(void* ptr, std::size_t size, std::size_t alignment) noexcept
	{
		alignment = (alignment < alignof(std::size_t)) ? alignof(std::size_t) : alignment;
		std::size_t const prev_offset = *reinterpret_cast<std::size_t*>(static_cast<std::byte*>(ptr) - sizeof(std::size_t));
		std::size_t const start_offset = (prev_offset + sizeof(std::size_t) + (alignment - 1)) & ~(alignment - 1);
		std::size_t const end_offset = (start_offset + size + (alignof(std::size_t) - 1)) & ~(alignof(std::size_t) - 1);
		if (end_offset != current_frame_block_->offset)
			std::terminate();
		current_frame_block_->offset = prev_offset;
		if ((prev_offset == 0) && (current_frame_block_ != frame_blocks_.begin()))
			--current_frame_block_;
	}

	template <class T>
	T* allocate()
	{
		return static_cast<T*>(allocate(sizeof(T), alignof(T)));
	}

	template <class T>
	void deallocate(T* ptr) noexcept
	{
		deallocate(ptr, sizeof(T), alignof(T));
	}
};

} // namespace detail

} // namespace lug

#endif
