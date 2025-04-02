// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#ifndef LUG_INCLUDE_LUG_STDIO_HPP
#define LUG_INCLUDE_LUG_STDIO_HPP

#include <lug/lug.hpp>

#include <cstdio>

#ifndef LUG_NO_ISATTY
#ifdef _MSC_VER
#ifndef LUG_HAS_ISATTY_MSVC
#define LUG_HAS_ISATTY_MSVC
#endif
#else
#ifndef LUG_HAS_ISATTY_POSIX
#ifdef __has_include
#if __has_include(<unistd.h>)
#define LUG_HAS_ISATTY_POSIX
#endif
#endif
#endif
#endif
#endif // LUG_NO_ISATTY

#if defined LUG_HAS_ISATTY_MSVC
#include <io.h>
#elif defined LUG_HAS_ISATTY_POSIX
#include <unistd.h>
#endif

#ifndef LUG_HAS_FLOCKFILE_POSIX
#if (defined _POSIX_C_SOURCE && _POSIX_C_SOURCE >= 1) || (defined _POSIX_SOURCE && _POSIX_SOURCE >= 1) || \
	(defined _XOPEN_SOURCE && _XOPEN_SOURCE >= 1) || (defined _BSD_SOURCE && _BSD_SOURCE >= 1) || (defined _SVID_SOURCE && _SVID_SOURCE >= 1)
#define LUG_HAS_FLOCKFILE_POSIX
#endif
#endif // LUG_HAS_FLOCKFILE_POSIX

#ifndef LUG_HAS_LOCK_FILE_MSVC
#ifdef _MSC_VER
#define LUG_HAS_LOCK_FILE_MSVC
#endif
#endif // LUG_HAS_LOCK_FILE_MSVC

namespace lug {

[[nodiscard]] inline bool file_isatty(std::FILE* file) noexcept
{
#if defined LUG_HAS_ISATTY_MSVC
	return (file != nullptr) && (_isatty(_fileno(file)) != 0);
#elif defined LUG_HAS_ISATTY_POSIX
	return (file != nullptr) && (isatty(fileno(file)) != 0);
#else
	return false;
#endif
}

[[nodiscard]] inline bool stdin_isatty() noexcept
{
	return file_isatty(stdin);
}

class std_file_locker
{
	std::FILE* file_{nullptr};

public:
	explicit std_file_locker(std::FILE* file) noexcept
		: file_{file}
	{
		if LUG_LIKELY(file_ != nullptr) {
#if defined LUG_HAS_FLOCKFILE_POSIX
			flockfile(file_);
#elif defined LUG_HAS_LOCK_FILE_MSVC
			_lock_file(file_);
#endif
		}
	}

	~std_file_locker() noexcept
	{
		unlock();
	}

	[[nodiscard]] bool is_locked() const noexcept
	{
		return file_ != nullptr;
	}

	void unlock() noexcept
	{
		if (file_ != nullptr) {
	#if defined LUG_HAS_FLOCKFILE_POSIX
			funlockfile(file_);
	#elif defined LUG_HAS_LOCK_FILE_MSVC
			_unlock_file(file_);
	#endif
			file_ = nullptr;
		}
	}

	std::FILE* release() noexcept
	{
		return std::exchange(file_, nullptr);
	}

	std_file_locker(std_file_locker const&) = delete;
	std_file_locker(std_file_locker&&) = delete;
	std_file_locker& operator=(std_file_locker const&) = delete;
	std_file_locker& operator=(std_file_locker&& other) = delete;
};

struct std_file_deleter
{
	void operator()(std::FILE* file) const noexcept
	{
		if (file != nullptr)
			(void)std::fclose(file); // NOLINT(cppcoreguidelines-owning-memory)
	}
};

using std_file_ptr = std::unique_ptr<std::FILE, std_file_deleter>;

[[nodiscard]] inline std_file_ptr fopen_unique(char const* filename, char const* mode) noexcept
{
#ifdef _MSC_VER
	std::FILE* file{nullptr};
	if (errno_t const err = fopen_s(&file, filename, mode); err != 0)
		return nullptr;
	return std_file_ptr{file};
#else
	return std_file_ptr{std::fopen(filename, mode)};
#endif
}

[[nodiscard]] inline int fgetc_unlocked(std::FILE* input) noexcept
{
#if defined LUG_HAS_FLOCKFILE_POSIX
	return getc_unlocked(input); // NOLINT(concurrency-mt-unsafe)
#elif defined LUG_HAS_LOCK_FILE_MSVC
	return _getc_nolock(input); // NOLINT(concurrency-mt-unsafe)
#else
	return std::fgetc(input);
#endif
}

[[nodiscard]] inline std::size_t fread_unlocked_s(void* buffer, [[maybe_unused]] std::size_t buffer_size, std::size_t element_size, std::size_t count, std::FILE* input) noexcept
{
#if defined LUG_HAS_FLOCKFILE_POSIX
	return fread_unlocked(buffer, element_size, count, input); // NOLINT(concurrency-mt-unsafe)
#elif defined LUG_HAS_LOCK_FILE_MSVC
	return _fread_nolock_s(buffer, buffer_size, element_size, count, input); // NOLINT(concurrency-mt-unsafe)
#else
	return std::fread(buffer, element_size, count, input);
#endif
}

inline std::FILE* skipline(std::FILE* input, int delim = '\n') noexcept
{
	if LUG_LIKELY(input != nullptr) {
		lug::std_file_locker const lock{input};
		for (;;) {
			int const ch{lug::fgetc_unlocked(input)};
			if ((ch == delim) || (ch == EOF))
				break;
		}
	}
	return input;
}

inline std::FILE* skipws(std::FILE* input) noexcept
{
	if LUG_LIKELY(input != nullptr) {
		lug::std_file_locker lock{input};
		for (;;) {
			int const ch{lug::fgetc_unlocked(input)};
			if (ch == EOF)
				break;
			if (std::isspace(ch) == 0) {
#if defined LUG_HAS_FLOCKFILE_POSIX
				lock.unlock();
				(void)ungetc(ch, input);
#elif defined LUG_HAS_LOCK_FILE_MSVC
				(void)_ungetc_nolock(ch, input); // NOLINT(concurrency-mt-unsafe)
				lock.unlock();
#else
				lock.unlock();
				(void)std::ungetc(ch, input);
#endif
				break;
			}
		}
	}
	return input;
}

template <class OutputIt>
bool readline(std::FILE* input, OutputIt output, int delim = '\n')
{
	if LUG_UNLIKELY(input == nullptr)
		return false;
	constexpr std::size_t max_count{(std::numeric_limits<std::size_t>::max)() / 2};
	std::size_t count{0};
	lug::std_file_locker const lock{input};
	while (count <= max_count) {
		int const ch = lug::fgetc_unlocked(input);
		if (ch == EOF)
			break;
		*output = static_cast<char>(ch);
		++output;
		++count;
		if (ch == delim)
			break;
	}
	return count > 0;
}

template <class OutputIt>
bool readfile(std::FILE* input, OutputIt output)
{
	if LUG_UNLIKELY(input == nullptr)
		return false;
	constexpr std::size_t max_count{(std::numeric_limits<std::size_t>::max)() / 2};
	std::size_t count{0};
	constexpr std::size_t buffer_size{4096};
	std::array<char, buffer_size> buffer{};
	lug::std_file_locker const lock{input};
	while (count <= max_count) {
		std::size_t const n{lug::fread_unlocked_s(buffer.data(), buffer.size(), 1, buffer.size(), input)};
		if (n == 0)
			break;
		output = std::copy_n(buffer.begin(), n, output);
		count += n;
		if (count < n)
			break;
	}
	return count > 0;
}

template <class OutputIt>
inline bool readsource(std::FILE* input, OutputIt output, int delim, source_options options = source_options::none)
{
	if ((options & source_options::interactive) != source_options::none)
		return lug::readline(input, output, delim);
	return lug::readfile(input, output);
}

template <class OutputIt>
inline bool readsource(std::FILE* input, OutputIt output, source_options options = source_options::none)
{
	return lug::readsource(input, output, '\n', options);
}

inline bool parse(std::FILE* input, grammar const& grmr, environment& envr, int delim, source_options options = source_options::none)
{
	return basic_parser<multi_input_source>{grmr, envr}.push_source([input, delim](auto output, source_options opt) -> bool {
		return static_cast<bool>(lug::readsource(input, output, delim, opt));
	}, options).parse();
}

inline bool parse(std::FILE* input, grammar const& grmr, environment& envr, source_options options = source_options::none)
{
	return parse(input, grmr, envr, '\n', options);
}

inline bool parse(std::FILE* input, grammar const& grmr, source_options opt = source_options::none)
{
	environment envr;
	return parse(input, grmr, envr, opt);
}

} // namespace lug

#endif
