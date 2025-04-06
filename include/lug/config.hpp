// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#ifndef LUG_INCLUDE_LUG_CONFIG_HPP
#define LUG_INCLUDE_LUG_CONFIG_HPP

// NOLINTBEGIN(cppcoreguidelines-macro-to-enum,cppcoreguidelines-macro-usage,modernize-macro-to-enum)

#define LUG_VERSION 0x000600
#define LUG_VERSION_MAJOR 0
#define LUG_VERSION_MINOR 6
#define LUG_VERSION_PATCH 0
#define LUG_VERSION_STRING "0.6.0"

#ifndef LUG_NO_EXCEPTIONS
#if defined __GNUC__
#ifndef __EXCEPTIONS
#define LUG_NO_EXCEPTIONS
#endif
#elif defined _MSC_VER
#ifndef _CPPUNWIND
#define LUG_NO_EXCEPTIONS
#endif
#endif
#endif

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
#endif

#if !defined LUG_LIKELY && (__cplusplus >= 202002L)
#if __has_cpp_attribute(likely)
#define LUG_LIKELY(...) (__VA_ARGS__) [[likely]]
#endif
#endif
#if !defined LUG_LIKELY && (defined __GNUC__ || defined __clang__)
#define LUG_LIKELY(...) (__builtin_expect((__VA_ARGS__), 1))
#endif
#ifndef LUG_LIKELY
#define LUG_LIKELY(...) (__VA_ARGS__)
#endif

#if !defined LUG_UNLIKELY && (__cplusplus >= 202002L)
#if __has_cpp_attribute(unlikely)
#define LUG_UNLIKELY(...) (__VA_ARGS__) [[unlikely]]
#endif
#endif
#if !defined LUG_UNLIKELY && (defined __GNUC__ || defined __clang__)
#define LUG_UNLIKELY(...) (__builtin_expect((__VA_ARGS__), 0))
#endif
#ifndef LUG_UNLIKELY
#define LUG_UNLIKELY(...) (__VA_ARGS__)
#endif

#if !defined LUG_ALWAYS_INLINE && !defined LUG_DEBUG && !defined _DEBUG && defined _MSC_VER
#define LUG_ALWAYS_INLINE __forceinline
#endif
#if !defined LUG_ALWAYS_INLINE && !defined LUG_DEBUG && !defined _DEBUG && (defined __GNUC__ || defined __clang__)
#if __has_attribute(always_inline)
#define LUG_ALWAYS_INLINE [[gnu::always_inline]]
#endif
#endif
#ifndef LUG_ALWAYS_INLINE
#define LUG_ALWAYS_INLINE
#endif

#if !defined LUG_ALLOC && defined _MSC_VER
#define LUG_ALLOC __declspec(allocator) __declspec(restrict)
#endif
#if !defined LUG_ALLOC && (defined __GNUC__ || defined __clang__)
#if __has_attribute(malloc)
#define LUG_ALLOC [[gnu::malloc]]
#endif
#endif
#ifndef LUG_ALLOC
#define LUG_ALLOC
#endif

#if !defined LUG_ALLOC_ALIGN && (defined __GNUC__ || defined __clang__)
#if __has_attribute(alloc_align)
#define LUG_ALLOC_ALIGN(Align) [[gnu::alloc_align(Align)]]
#endif
#endif
#ifndef LUG_ALLOC_ALIGN
#define LUG_ALLOC_ALIGN(Align)
#endif

#if !defined LUG_ALLOC_SIZE && (defined __GNUC__ || defined __clang__)
#if __has_attribute(alloc_size)
#define LUG_ALLOC_SIZE(Size) [[gnu::alloc_size(Size)]]
#endif
#endif
#ifndef LUG_ALLOC_SIZE
#define LUG_ALLOC_SIZE(Size)
#endif

#if !defined LUG_MUSTTAIL && defined __clang__
#if __has_attribute(musttail)
#define LUG_MUSTTAIL [[clang::musttail]]
#endif
#endif
#if !defined LUG_MUSTTAIL && defined __GNUC__
#if __has_attribute(musttail)
#define LUG_MUSTTAIL [[gnu::musttail]]
#endif
#endif
#ifndef LUG_MUSTTAIL
#define LUG_MUSTTAIL
#endif

#if !defined LUG_NONNULL && (defined __GNUC__ || defined __clang__)
#if __has_attribute(nonnull)
#define LUG_NONNULL(...) [[gnu::nonnull(__VA_ARGS__)]]
#endif
#endif
#ifndef LUG_NONNULL
#define LUG_NONNULL(...)
#endif

#if !defined LUG_RETURNS_NONNULL && (defined __GNUC__ || defined __clang__)
#if __has_attribute(returns_nonnull)
#define LUG_RETURNS_NONNULL [[gnu::returns_nonnull]]
#endif
#endif
#ifndef LUG_RETURNS_NONNULL
#define LUG_RETURNS_NONNULL
#endif

#ifndef LUG_DIAGNOSTIC_PUSH_AND_IGNORE
#ifdef __GNUC__
#define LUG_DIAGNOSTIC_PUSH_AND_IGNORE \
_Pragma("GCC diagnostic push") \
_Pragma("GCC diagnostic ignored \"-Wparentheses\"") \
_Pragma("GCC diagnostic ignored \"-Wlogical-not-parentheses\"") \
_Pragma("GCC diagnostic ignored \"-Wuninitialized\"") \
_Pragma("GCC diagnostic ignored \"-Wunused-variable\"")
_Pragma("GCC diagnostic ignored \"-Wunused-but-set-variable\"")
#else
#define LUG_DIAGNOSTIC_POP
#endif
#endif

#ifndef LUG_DIAGNOSTIC_POP
#ifdef __GNUC__
#define LUG_DIAGNOSTIC_POP _Pragma("GCC diagnostic pop")
#else
#define LUG_DIAGNOSTIC_POP
#endif
#endif

// NOLINTEND(cppcoreguidelines-macro-to-enum,cppcoreguidelines-macro-usage,modernize-macro-to-enum)

#endif // LUG_INCLUDE_LUG_CONFIG_HPP
