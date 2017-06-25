// lug - Embedded DSL for PE grammar parsers in C++
// Copyright (c) 2017 Jesse W. Towner

#ifndef LUG_HPP
#define LUG_HPP

#include <any>
#include <array>
#include <algorithm>
#include <functional>
#include <iostream>
#include <locale>
#include <memory>
#include <numeric>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace lug
{

namespace detail
{

template <class T>
struct dynamic_cast_if_base_of
{
	std::remove_reference_t<T>& b;
	template <class U, class = std::enable_if_t<std::is_base_of_v<std::decay_t<T>, std::decay_t<U>>>>
	operator U&() const volatile { return dynamic_cast<std::remove_reference_t<U>&>(b); }
};

template <class Error>
struct reentrancy_sentinel
{
	bool& value;
	reentrancy_sentinel(bool& x, const char* msg) : value{x} { if (value) throw Error{msg}; value = true; }
	~reentrancy_sentinel() { value = false; }
};

} // namespace detail

namespace utf8
{

constexpr bool is_lead(char c) noexcept {
	return (static_cast<unsigned char>(c) & 0xC0) != 0x80;
}

template <class InputIt>
constexpr std::size_t count_runes(InputIt first, InputIt last) {
	return ::std::accumulate(first, last, std::size_t{0}, [](std::size_t l, char c) { return l + is_lead(c); });
}

template <class InputIt>
constexpr InputIt next_rune(InputIt first, InputIt last) {
	return first != last ? ::std::find_if(::std::next(first), last, is_lead) : last;
}

template <class InputIt>
constexpr std::size_t size_of_first_rune(InputIt first, InputIt last) {
	return static_cast<std::size_t>(::std::distance(first, next_rune(first, last)));
}

} // namespace utf8

struct program; template <class Target> struct call_expression;
class rule; class grammar; class encoder; class rule_encoder; class parser; class semantic_environment;
class lug_error : public std::runtime_error { using std::runtime_error::runtime_error; };
class grammar_error : public lug_error { using lug_error::lug_error; };
class parser_error : public lug_error { using lug_error::lug_error; };
struct syntax_position { std::size_t column, line; };
struct syntax_view { std::string_view capture; syntax_position start, end; };
typedef std::function<bool(parser&)> semantic_predicate;
typedef std::function<void(semantic_environment&)> semantic_action;
typedef std::function<void(semantic_environment&, const syntax_view&)> syntax_action;
template <class E> constexpr bool is_callable_v = std::is_same_v<grammar, std::decay_t<E>> || std::is_same_v<rule, std::decay_t<E>> || std::is_same_v<program, std::decay_t<E>>;
template <class E> constexpr bool is_proper_expression_v = std::is_invocable_v<E, encoder&>;
template <class E> constexpr bool is_string_expression_v = std::is_convertible_v<E, std::string>;
template <class E> constexpr bool is_expression_v = is_callable_v<E> || is_proper_expression_v<E> || is_string_expression_v<E> || std::is_same_v<char, E>;

enum class immediate : unsigned short {};
enum class operands : unsigned char { none = 0, off = 1, str = 2 };
constexpr operands operator&(operands x, operands y) noexcept { return static_cast<operands>(static_cast<unsigned char>(x) & static_cast<unsigned char>(y)); }
constexpr operands operator|(operands x, operands y) noexcept { return static_cast<operands>(static_cast<unsigned char>(x) | static_cast<unsigned char>(y)); }

enum class opcode : unsigned char
{
	match,          match_any,      match_class,    match_range,
	choice,         commit,         jump,           call,
	ret,            fail,           accept,         newline,
	predicate,      action,         begin_capture,  end_capture
};

union instruction
{
	static constexpr std::size_t maxstrlen = 256;
	struct prefix { opcode op; operands aux; unsigned short val; } pf;
	int off;
	std::array<char, 4> str;

	instruction(opcode op, operands aux, immediate imm) : pf{op, aux, static_cast<unsigned short>(imm)} {}
	instruction(std::ptrdiff_t o) : off{static_cast<int>(o)} { if (off != o) throw grammar_error{"offset exceeds allowable range"}; }
	instruction(std::string_view s) { std::fill(std::copy_n(s.begin(), (std::min)(s.size(), std::size_t{4}), str.begin()), str.end(), char{0}); }

	static opcode decode(const std::vector<instruction>& code, std::ptrdiff_t& pc, std::size_t& imm, std::ptrdiff_t& off, std::string_view& str) {
		const prefix pf = code[pc++].pf;
		imm = pf.val, off = (pf.aux & operands::off) == operands::off ? code[pc++].off : 0;
		if ((pf.aux & operands::str) == operands::str) {
			str = std::string_view{code[pc].str.data(), (imm & 0xFF) + 1};
			pc += ((imm & 0xFF) + 4) >> 2;
			imm = (imm >> 8) + 1;
		} else {
			str = std::string_view{};
		}
		return pf.op;
	}

	static std::ptrdiff_t length(prefix p) noexcept {
		std::ptrdiff_t len = 1;
		if ((p.aux & operands::off) == operands::off)
			len++;
		if ((p.aux & operands::str) == operands::str)
			len += static_cast<std::ptrdiff_t>(((p.val & 0xFF) >> 2) + 1);
		return len;
	}
};

static_assert(sizeof(instruction) == sizeof(int), "expected instruction to be same size as int");
static_assert(sizeof(int) <= sizeof(std::ptrdiff_t), "expected int to be no larger than ptrdiff_t");
static_assert(sizeof(std::ctype_base::mask) <= sizeof(immediate), "immediate must be large enough to hold std::ctype::mask");

struct program
{
	std::vector<instruction> instructions;
	std::vector<semantic_predicate> predicates;
	std::vector<semantic_action> actions;
	std::vector<syntax_action> syntax_actions;

	void concatenate(const program& src) {
		instructions.reserve(instructions.size() + src.instructions.size());
		for (auto i = src.instructions.begin(), j = i, e = src.instructions.end(); i != e; i = j) {
			instruction instr = *i;
			std::size_t valoffset;
			switch (instr.pf.op) {
				case opcode::predicate: valoffset = predicates.size(); break;
				case opcode::action: valoffset = actions.size(); break;
				case opcode::end_capture: valoffset = syntax_actions.size(); break;
				default: valoffset = 0; break;
			}
			if (valoffset != 0) {
				std::size_t val = instr.pf.val + valoffset;
				if (val < valoffset || val > (std::numeric_limits<unsigned short>::max)())
					throw grammar_error("immediate value exceeds 16-bit limit");
				instr.pf.val = static_cast<unsigned short>(val);
			}
			j = std::next(i, instruction::length(instr.pf));
			instructions.push_back(instr);
			instructions.insert(instructions.end(), i + 1, j);
		}
		predicates.insert(predicates.end(), src.predicates.begin(), src.predicates.end());
		actions.insert(actions.end(), src.actions.begin(), src.actions.end());
		syntax_actions.insert(syntax_actions.end(), src.syntax_actions.begin(), src.syntax_actions.end());
	}

	void swap(program& p) {
		instructions.swap(p.instructions);
		predicates.swap(p.predicates);
		actions.swap(p.actions);
		syntax_actions.swap(p.syntax_actions);
	}
};

class rule
{
	friend class encoder;
	friend class rule_encoder;
	friend grammar start(const rule&);
	lug::program program_;
	std::vector<std::tuple<const lug::rule*, const lug::program*, std::ptrdiff_t>> callees_;
public:
	rule() = default;
	template <class E, class = std::enable_if_t<is_expression_v<E>>> rule(const E& e);
	rule(const rule& r);
	rule(rule&& r) = default;
	rule& operator=(const rule& r) { rule{r}.swap(*this); return *this; }
	rule& operator=(rule&& r) = default;
	void swap(rule& r) { program_.swap(r.program_); callees_.swap(r.callees_); }
	call_expression<rule> operator()(unsigned short precedence);
};

class grammar
{
	friend grammar start(const rule&);
	lug::program program_;
	grammar(lug::program p) : program_{std::move(p)} {}
public:
	grammar() = default;
	void swap(grammar& g) { program_.swap(g.program_); }
	const lug::program& program() const noexcept { return program_; };
};

class semantic_environment
{
	std::string_view capture_;
	std::vector<semantic_action> actions_;
	std::vector<std::any> attributes_;
	std::vector<std::unordered_map<void*, std::function<void(void*)>>> vframes_;
	virtual void on_accept_begin() {}
	virtual void on_accept_end() {}
	virtual void on_clear() {}
public:
	virtual ~semantic_environment() {}
	const std::string_view& capture() const { return capture_; }
	std::size_t action_count() const noexcept { return actions_.size(); }
	void pop_actions_after(std::size_t n) { if (n < actions_.size()) actions_.resize(n); }
	auto push_action(semantic_action a) { actions_.push_back(std::move(a)); return action_count(); }
	template <class T> void push_attribute(T&& x) { attributes_.emplace_back(std::in_place_type<T>, ::std::forward<T>(x)); }
	template <class T, class... Args> void push_attribute(Args&&... args) { attributes_.emplace_back(std::in_place_type<T>, ::std::forward<Args>(args)...); }
	template <class T> T pop_attribute() { T r{::std::any_cast<T>(attributes_.back())}; attributes_.pop_back(); return r; }
	void enter_frame() { vframes_.emplace_back(); }
	void leave_frame() { std::for_each(vframes_.back().begin(), vframes_.back().end(), [](auto& entry) { entry.second(entry.first); }); vframes_.pop_back(); }

	void accept(std::string_view c) {
		capture_ = c;
		on_accept_begin();
		for (auto& a : actions_)
			a(*this);
		actions_.clear();
		on_accept_end();
	}

	void clear() {
		capture_ = std::string_view{};
		actions_.clear();
		attributes_.clear();
		on_clear();
	}

	auto drop_actions_after(std::size_t n) {
		std::vector<semantic_action> dropped;
		if (n < actions_.size()) {
			dropped.assign(std::make_move_iterator(actions_.begin() + n), std::make_move_iterator(actions_.end()));
			actions_.resize(n);
		}
		return dropped;
	}

	auto restore_actions_after(std::size_t n, const std::vector<semantic_action>& a) {
		pop_actions_after(n);
		actions_.insert(actions_.end(), a.begin(), a.end());
		return action_count();
	}

	template <class T>
	void save_variable(T& x) {
		if (!vframes_.empty()) {
			auto& vframe = vframes_.back();
			if (vframe.count(&x) == 0)
				vframe.emplace(&x, [v = std::decay_t<T>{x}](void* p) { *static_cast<std::decay_t<T>*>(p) = v; });
		}
	}
};

class encoder
{
	virtual immediate do_add_predicate(semantic_predicate) { return immediate{0}; }
	virtual immediate do_add_semantic_action(semantic_action) { return immediate{0}; }
	virtual immediate do_add_syntax_action(syntax_action) { return immediate{0}; }
	virtual void do_add_callee(const rule*, const program*, std::ptrdiff_t) {}
	virtual void do_append(instruction) = 0;
	virtual std::ptrdiff_t do_length() const noexcept = 0;
public:
	virtual ~encoder() {}
	encoder& call(const program& p, unsigned short prec) { do_add_callee(nullptr, &p, length()); return encode(opcode::call, 0, immediate{prec}); }
	encoder& call(const rule& r, unsigned short prec) { do_add_callee(&r, &r.program_, length()); return encode(opcode::call, 0, immediate{prec}); }
	encoder& call(const grammar& g, unsigned short prec) { do_add_callee(nullptr, &g.program(), length()); return encode(opcode::call, 3, immediate{prec}); }
	encoder& encode(opcode op, immediate imm = immediate{0}) { return append(instruction{op, operands::none, imm}); }
	encoder& encode(opcode op, semantic_predicate p) { return append(instruction{op, operands::none, do_add_predicate(std::move(p))}); }
	encoder& encode(opcode op, semantic_action a) { return append(instruction{op, operands::none, do_add_semantic_action(std::move(a))}); }
	encoder& encode(opcode op, syntax_action a) { return append(instruction{op, operands::none, do_add_syntax_action(std::move(a))}); }
	encoder& encode(opcode op, std::ptrdiff_t off, immediate imm = immediate{0}) { return append(instruction{op, operands::off, imm}).append(instruction{off}); }
	template <class E> auto evaluate(const E& e) -> std::enable_if_t<is_expression_v<E>, encoder&>;
	std::ptrdiff_t length() const noexcept { return do_length(); }

	encoder& append(instruction instr) {
		do_append(instr);
		return *this;
	}

	template <class InputIt>
	encoder& append(InputIt first, InputIt last) {
		for ( ; first != last; ++first)
			do_append(*first);
		return *this;
	}

	encoder& encode(opcode op, std::size_t val, std::string_view subsequence) {
		if (!subsequence.empty()) {
			if (val == 0 || val > 256 || subsequence.size() > instruction::maxstrlen)
				throw grammar_error{"invalid immediate value or subsequence exceeds allowed number of UTF8 octets"};
			do_append(instruction{op, operands::str, static_cast<immediate>(((val - 1) << 8) | (subsequence.size() - 1))});
			do {
				do_append(instruction{subsequence});
				subsequence.remove_prefix((std::min)(size_t{4}, subsequence.size()));
			} while (!subsequence.empty());
		}
		return *this;
	}

	encoder& match(std::string_view sequence) {
		while (sequence.size() > instruction::maxstrlen) {
			std::string_view subsequence = sequence.substr(0, instruction::maxstrlen);
			while (!subsequence.empty() && !utf8::is_lead(subsequence.back()))
				subsequence.remove_suffix(1);
			subsequence.remove_suffix(!subsequence.empty());
			encode(opcode::match, utf8::count_runes(subsequence.cbegin(), subsequence.cend()), subsequence);
			sequence.remove_prefix(subsequence.size());
		}
		return encode(opcode::match, utf8::count_runes(sequence.cbegin(), sequence.cend()), sequence);
	}

	encoder& match_range(std::string_view first, std::string_view last) {
		return encode(opcode::match_range, first.size(), std::string{first}.append(last));
	}
};

class instruction_length_evaluator : public encoder
{
	std::ptrdiff_t length_ = 0;
	std::ptrdiff_t do_length() const noexcept override { return length_; }

	void do_append(instruction instr) override {
		if (length_ >= (std::numeric_limits<std::ptrdiff_t>::max)())
			throw grammar_error{"program length exceeds limits"};
		++length_;
	}
};

template <class E>
std::ptrdiff_t instruction_length(const E& e) {
	return instruction_length_evaluator{}.evaluate(e).length();
}

class instruction_encoder : public encoder
{
std::vector<instruction>& instructions_;
std::ptrdiff_t do_length() const noexcept override { return static_cast<std::ptrdiff_t>(instructions_.size()); }

void do_append(instruction instr) override {
	if (instructions_.size() >= static_cast<size_t>((std::numeric_limits<std::ptrdiff_t>::max)()))
		throw grammar_error{"program length exceeds limits"};
	instructions_.push_back(instr);
}

public:
	explicit instruction_encoder(std::vector<instruction>& i) : instructions_{i} {}
};

class program_encoder : public instruction_encoder
{
	program& program_;
	immediate do_add_predicate(semantic_predicate p) override { return add_item(program_.predicates, std::move(p)); }
	immediate do_add_semantic_action(semantic_action a) override { return add_item(program_.actions, std::move(a)); }
	immediate do_add_syntax_action(syntax_action a) override { return add_item(program_.syntax_actions, std::move(a)); }

	template <class Item>
	immediate add_item(std::vector<Item>& items, Item&& item) {
		if (items.size() >= (std::numeric_limits<unsigned short>::max)())
			throw grammar_error{"number of objects exceeds internal limit"};
		items.push_back(::std::forward<Item>(item));
		return static_cast<immediate>(items.size() - 1);
	}

public:
	explicit program_encoder(program& p) : instruction_encoder{p.instructions}, program_{p} {}
};

class rule_encoder : public program_encoder
{
	rule& rule_;
	void do_add_callee(const rule* r, const program* p, std::ptrdiff_t n) override { rule_.callees_.emplace_back(r, p, n); }
public:
	explicit rule_encoder(rule& r) : program_encoder{r.program_}, rule_{r} {}
};

struct accept_action { void operator()(encoder& v) const { v.encode(opcode::accept); } };
struct newline_action { void operator()(encoder& v) const { v.encode(opcode::newline); } };
struct any_terminal { void operator()(encoder& v) const { v.encode(opcode::match_any); } };
struct empty_terminal { void operator()(encoder& v) const { v.encode(opcode::match); } };
struct char_terminal { char c; void operator()(encoder& v) const { v.match(std::string_view{&c, 1}); } };

template <class Target>
struct call_expression
{
	const Target& target;
	unsigned short precedence;
	void operator()(encoder& v) const { v.call(target, precedence); }
};

class string_expression
{
	std::vector<instruction> instructions_;
	static grammar make_grammar();
	void compile(const std::string& s);

	struct generator : public semantic_environment
	{
		instruction_encoder encoder;
		bool circumflex;
		std::ctype_base::mask classes;
		std::vector<std::pair<std::string_view, std::string_view>> ranges;
		generator(string_expression& se) : encoder{se.instructions_}, circumflex{false}, classes{0} {}

		void bracket_class(std::string_view s) {
			using ct = std::ctype_base; static constexpr std::pair<const char* const, ct::mask> m[12] = {
				{"alnum", ct::alnum}, {"alpha", ct::alpha}, {"blank", ct::blank}, {"cntrl", ct::cntrl},
				{"digit", ct::digit}, {"graph", ct::graph}, {"lower", ct::lower}, {"print", ct::print},
				{"punct", ct::punct}, {"space", ct::space}, {"upper", ct::upper}, {"xdigit", ct::xdigit}};
			auto c = std::find_if(std::begin(m), std::end(m), [s](auto& x) { return !s.compare(x.first); });
			if (c == std::end(m))
				throw grammar_error{"invalid character class"};
			classes |= c->second;
		}

		void bracket_range(std::string_view s) {
			bracket_range(s.substr(0, s.find('-')), s.substr(s.find('-') + 1));
		}

		void bracket_range(std::string_view first, std::string_view last) {
			ranges.emplace_back(first > last ? last : first, first > last ? first : last);
			std::push_heap(ranges.begin(), ranges.end(), [](auto& a, auto& b) { return a.first < b.first; });
		}

		void bracket_commit() {
			std::vector<instruction> matches;
			if (!ranges.empty()) {
				std::vector<std::pair<std::string_view, std::string_view>> merged;
				std::sort_heap(ranges.begin(), ranges.end(), [](auto& a, auto& b) { return a.first < b.first; });
				for (auto curr = merged.end(), next = ranges.begin(), last = ranges.end(); next != last; ++next) {
					if (curr == merged.end() || next->first < curr->first || curr->second < next->first)
						curr = merged.insert(merged.end(), *next); 
					else
						curr->second = curr->second < next->second ? next->second : curr->second;
				}
				if (auto curr = merged.crbegin(), last = merged.crend(); curr != last) {
					instruction_encoder{matches}.match_range(curr->first, curr->second);
					for (++curr; curr != last; ++curr) {
						std::vector<instruction> left, both;
						instruction_encoder{left}.match_range(curr->first, curr->second);
						instruction_encoder{both}
							.encode(opcode::choice, 2 + left.size()).append(left.begin(), left.end())
							.encode(opcode::commit, matches.size()).append(matches.begin(), matches.end());
						matches = std::move(both);
					}
				}
			}
			if (circumflex)
				encoder.encode(opcode::choice, 3 + matches.size() + (classes ? 1 : 0));
			encoder.append(matches.begin(), matches.end());
			if (classes)
				encoder.encode(opcode::match_class, immediate{static_cast<unsigned short>(classes)});
			if (circumflex)
				encoder.encode(opcode::commit, 0).encode(opcode::fail);
			circumflex = false;
			classes = 0;
		}
	};

public:
	string_expression(const std::string& s) { compile(s); }
	void operator()(encoder& v) const { v.append(instructions_.begin(), instructions_.end()); }
};

template <class T>
inline auto make_expression(const T& t) {
	static_assert(is_expression_v<T>, "T must be an expression type");
	if constexpr (is_callable_v<T>)
		return call_expression<T>{t, 0};
	else if constexpr (std::is_same_v<char, T>)
		return char_terminal{t};
	else if constexpr (is_string_expression_v<T>)
		return string_expression{t};
	else
		return t;
}

template <class E>
inline auto encoder::evaluate(const E& e) -> std::enable_if_t<is_expression_v<E>, encoder&> {
	make_expression(e)(*this);
	return *this;
}

template <class E, class> inline rule::rule(const E& e) { rule_encoder{*this}.evaluate(e); }
inline rule::rule(const rule& r) { rule_encoder{*this}.call(r, 0); /* TODO: jump instead of call */ }
inline call_expression<rule> rule::operator()(unsigned short precedence) { return call_expression<rule>{*this, precedence}; }

namespace language
{

using semantics = lug::semantic_environment&; using syntax = const lug::syntax_view&;
using lug::grammar; using lug::rule; using lug::start;
using lug::accept_action; using lug::newline_action; using lug::any_terminal; using lug::char_terminal; using lug::empty_terminal;
using namespace std::literals::string_literals;

template <class E, class = std::enable_if_t<is_expression_v<E>>>
inline auto operator!(E&& e) {
	return [x = make_expression(::std::forward<E>(e))](encoder& d) {
		d.encode(opcode::choice, 3 + instruction_length(x)).evaluate(x).encode(opcode::commit, 0).encode(opcode::fail); };
}

template <class E, class = std::enable_if_t<is_expression_v<E>>>
inline auto operator*(E&& e) {
	return [x = make_expression(::std::forward<E>(e))](encoder& d) {
		auto x_length = instruction_length(x);
		d.encode(opcode::choice, 2 + x_length).evaluate(x).encode(opcode::commit, -(x_length + 4)); };
}

template <class E1, class E2, class = std::enable_if_t<is_expression_v<E1> && is_expression_v<E2>>>
inline auto operator|(E1&& e1, E2&& e2) {
	return [x1 = make_expression(::std::forward<E1>(e1)), x2 = make_expression(::std::forward<E2>(e2))](encoder& d) {
		d.encode(opcode::choice, 2 + instruction_length(x1)).evaluate(x1).encode(opcode::commit, instruction_length(x2)).evaluate(x2); };
}

template <class E1, class E2, class = std::enable_if_t<is_expression_v<E1> && is_expression_v<E2>>>
inline auto operator>(E1&& e1, E2&& e2) {
	return [x1 = make_expression(::std::forward<E1>(e1)), x2 = make_expression(::std::forward<E2>(e2))](encoder& d) {
		d.evaluate(x1).evaluate(x2); };
}

template <class E, class A, class = std::enable_if_t<is_expression_v<E>>>
inline auto operator<(E&& e, A a) {
	if constexpr (std::is_invocable_v<A, semantics, syntax>) {
		return [x = make_expression(::std::forward<E>(e)), a = ::std::move(a)](encoder& d) {
			d.encode(opcode::begin_capture).evaluate(x).encode(opcode::end_capture, syntax_action{a}); };
	} else if constexpr (std::is_invocable_v<A, detail::dynamic_cast_if_base_of<semantics>, syntax>) {
		return ::std::forward<E>(e) < [a = std::move(a)](semantics s, syntax x) { a(detail::dynamic_cast_if_base_of<semantics>{s}, x); };
	} else if constexpr (std::is_invocable_v<A, semantics>) {
		return [x = make_expression(::std::forward<E>(e)), a = ::std::move(a)](encoder& d) {
			d.evaluate(x).encode(opcode::action, semantic_action{a}); };
	} else if constexpr (std::is_invocable_v<A, detail::dynamic_cast_if_base_of<semantics>>) {
		return ::std::forward<E>(e) < [a = std::move(a)](semantics s) { a(detail::dynamic_cast_if_base_of<semantics>{s}); };
	} else if constexpr (std::is_invocable_v<A> && std::is_same_v<void, std::invoke_result_t<A>>) {
		return [x = make_expression(::std::forward<E>(e)), a = ::std::move(a)](encoder& d) {
			d.evaluate(x).encode(opcode::action, [a](semantics s) { a(); }); };
	} else if constexpr (std::is_invocable_v<A>) {
		return [x = make_expression(::std::forward<E>(e)), a = ::std::move(a)](encoder& d) {
			d.evaluate(x).encode(opcode::action, [a](semantics s) { s.push_attribute(a()); }); };
	}
}

template <class T, class E, class = std::enable_if_t<is_expression_v<E>>>
inline auto operator<<(T& v, E&& e) {
	return ::std::forward<E>(e) < [&v](semantics s, syntax x) { s.save_variable(v); v = T{x.capture}; };
}

template <class T, class E, class = std::enable_if_t<is_expression_v<E>>>
inline auto operator%(T& v, E&& e) {
	return ::std::forward<E>(e) < [&v](semantics s) { s.save_variable(v); v = s.pop_attribute<T>(); };
}

template <class E, class = std::enable_if_t<is_expression_v<E>>> inline auto operator&(E&& e) { return !(!(::std::forward<E>(e))); }
template <class E, class = std::enable_if_t<is_expression_v<E>>> inline auto operator+(E&& e) { return ::std::forward<E>(e) > *(::std::forward<E>(e)); }
template <class E, class = std::enable_if_t<is_expression_v<E>>> inline auto operator~(E&& e) { return ::std::forward<E>(e) | empty_terminal{}; }

} // namespace language

inline grammar start(const rule& start_rule) {
	program gp;
	std::unordered_map<const program*, std::ptrdiff_t> addresses;
	std::vector<std::pair<const program*, std::ptrdiff_t>> calls;
	std::unordered_set<const program*> recursive;
	std::vector<std::pair<std::vector<const rule*>, const program*>> unprocessed;
	program_encoder{gp}.encode(opcode::call, 0, immediate{0}).encode(opcode::accept, immediate{1});
	calls.emplace_back(&start_rule.program_, 0);
	unprocessed.emplace_back(std::vector<const rule*>{&start_rule}, &start_rule.program_);
	do {
		auto [rules, subprogram] = unprocessed.back();
		unprocessed.pop_back();
		auto address = static_cast<std::ptrdiff_t>(gp.instructions.size());
		if (addresses.emplace(subprogram, address).second) {
			gp.concatenate(*subprogram);
			gp.instructions.emplace_back(opcode::ret, operands::none, immediate{0});
			if (auto r = rules.back(); r) {
				for (auto [cr, cp, instr_offset] : r->callees_) {
					calls.emplace_back(cp, address + instr_offset);
					if (std::find(rules.crbegin(), rules.crend(), cr) != rules.crend()) {
						recursive.insert(cp);
					} else {
						rules.push_back(cr);
						unprocessed.emplace_back(rules, cp);
						rules.pop_back();
					}
				}
			}
		}
	} while (!unprocessed.empty());
	for (auto [subprogram, instr_addr] : calls) {
		if (auto& iprefix = gp.instructions[instr_addr]; iprefix.pf.op == opcode::call)
			iprefix.pf.val = recursive.count(subprogram) != 0 ? (iprefix.pf.val != 0 ? iprefix.pf.val : 1) : 0;
		auto& ioffset = gp.instructions[instr_addr + 1];
		std::ptrdiff_t reladdr = ioffset.off + addresses[subprogram] - (instr_addr + 2);
		if (reladdr < std::numeric_limits<int>::lowest() || (std::numeric_limits<int>::max)() < reladdr)
			throw grammar_error("program offset exceeds addressable range");
		ioffset.off = static_cast<int>(reladdr);
	}
	return grammar(std::move(gp));
}

class parser
{
	enum class stack_frame_type : unsigned char { backtrack, call, lrcall, capture };
	struct syntax_state { std::size_t index; syntax_position position; };
	struct program_state { std::size_t action_counter; std::ptrdiff_t program_counter; };
	struct lrmemo_state { std::size_t acr; std::ptrdiff_t pcr, pca; syntax_state sr, sa; std::vector<semantic_action> actions; std::size_t prec; };
	static constexpr std::size_t lrfailcode = (std::numeric_limits<std::size_t>::max)();

	const lug::grammar& grammar_;
	semantic_environment& semantics_;
	std::locale locale_;
	std::string input_;
	syntax_state input_state_{0, 1, 1};
	program_state program_state_{0, 0};
	bool parsing_{false}, reading_{false};
	std::vector<std::function<bool(std::string&)>> sources_;
	std::vector<stack_frame_type> stack_frames_;
	std::vector<std::pair<program_state, syntax_state>> backtrack_stack_;
	std::vector<program_state> call_stack_;
	std::vector<syntax_state> capture_stack_;
	std::vector<lrmemo_state> lrcall_stack_;

	void load_registers(const syntax_state& ss, const program_state& ps, std::size_t& ir, std::size_t& cr, std::size_t& lr, std::size_t& ac, std::ptrdiff_t& pc) {
		ir = ss.index, cr = ss.position.column, lr = ss.position.line;
		ac = ps.action_counter, pc = ps.program_counter;
		semantics_.pop_actions_after(ac);
	}

	bool available(std::size_t n, std::size_t ir) {
		do {
			if (n <= input_.size() - ir)
				return true;
			if (ir < input_.size())
				return false;
		} while (read_more());
		return false;
	}

	bool read_more() {
		detail::reentrancy_sentinel<parser_error> guard{reading_, "lug::parser::read_more is non-reenterant"};
		std::string text;
		while (!sources_.empty() && text.empty()) {
			bool more = sources_.back()(text);
			input_.insert(input_.end(), text.begin(), text.end());
			if (!more)
				sources_.pop_back();
		}
		return !text.empty();
	}

public:
	parser(const grammar& g, semantic_environment& s) : grammar_{g}, semantics_{s} {}
	std::string_view input_view() const noexcept { return std::string_view{ input_.data() + input_state_.index, input_.size() - input_state_.index }; }
	const syntax_position& input_position() const noexcept { return input_state_.position; }
	void input_position(const syntax_position& position) noexcept { input_state_.position = position; }
	void input_position(std::size_t column, std::size_t line) noexcept { input_position(syntax_position{ column, line }); }

	void save_registers(std::size_t ir, std::size_t cr, std::size_t lr, std::size_t ac, std::ptrdiff_t pc) {
		input_state_ = { ir,{ cr, lr } }, program_state_ = { ac, pc };
	}

	void load_registers(std::size_t& ir, std::size_t& cr, std::size_t& lr, std::size_t& ac, std::ptrdiff_t& pc) {
		load_registers(input_state_, program_state_, ir, cr, lr, ac, pc);
	}

	template <class InputIt>
	parser& enqueue(InputIt first, InputIt last) {
		input_.insert(input_.end(), first, last);
		return *this;
	}

	template <class InputFunc>
	parser& push_source(InputFunc&& func) {
		if (reading_)
			throw parser_error("new input source cannot be specified while reading from input sources");
		sources_.emplace_back(::std::forward<InputFunc>(func));
		return *this;
	}

	bool parse() {
		detail::reentrancy_sentinel<parser_error> guard{parsing_, "lug::parser::parse is non-reenterant"};
		const program& prog = grammar_.program();
		bool result = false, done = false;
		std::size_t imm, ir, cr, lr, ac, fc = 0;
		std::ptrdiff_t off, pc;
		std::string_view str;
		semantics_.clear();
		program_state_ = {0, 0};
		load_registers(ir, cr, lr, ac, pc);
		while (!done) {
			switch (instruction::decode(prog.instructions, pc, imm, off, str)) {
				case opcode::match: {
					if (str.size() > 0) {
						if (!available(str.size(), ir) || input_.compare(ir, str.size(), str) != 0)
							goto failure;
						ir += str.size(), cr += imm;
					}
				} break;
				case opcode::match_any: {
					if (!available(1, ir))
						goto failure;
					ir += utf8::size_of_first_rune(input_.cbegin() + ir, input_.cend()), ++cr;
				} break;
				case opcode::match_class: {
					auto sz = utf8::size_of_first_rune(input_.cbegin() + ir, input_.cend());
					try {
						const auto& codecvt = std::use_facet<std::codecvt<wchar_t, char, std::mbstate_t>>(locale_);
						std::wstring_convert<std::codecvt<wchar_t, char, std::mbstate_t>, wchar_t> cvt(&codecvt);
						auto wc = cvt.from_bytes(input_.data() + ir, input_.data() + ir + sz);
						if (wc.empty() || !std::use_facet<std::ctype<wchar_t>>(locale_).is(static_cast<short>(imm), wc[0]))
							goto failure;
					} catch (std::exception&) {
						goto failure;
					}
					ir += sz, ++cr;
				} break;
				case opcode::match_range: {
					std::string_view first = str.substr(0, imm), last = str.substr(imm);
					if (!available((std::min)(first.size(), last.size()), ir))
						goto failure;
					auto sz = utf8::size_of_first_rune(input_.cbegin() + ir, input_.cend());
					if (input_.compare(ir, sz, first) < 0 || input_.compare(ir, sz, last) > 0)
						goto failure;
					ir += sz, ++cr;
				} break;
				case opcode::choice: {
					stack_frames_.push_back(stack_frame_type::backtrack);
					backtrack_stack_.push_back({{ac,pc+off},{ir-(imm&255),cr-(imm>>8),lr}});
				} break;
				case opcode::commit: {
					if (stack_frames_.empty() || stack_frames_.back() != stack_frame_type::backtrack)
						goto failure;
					backtrack_stack_.pop_back();
					stack_frames_.pop_back();
				} [[fallthrough]];
				case opcode::jump: {
					pc += off;
				} break;
				case opcode::call: {
					if (imm != 0) {
						if (auto memo = std::find_if(lrcall_stack_.crbegin(), lrcall_stack_.crend(),
									[pca = pc + off, ir](auto& x) { return pca == x.pca && ir == x.sr.index; });
								memo != lrcall_stack_.crend()) {
							if (memo->sa.index == lrfailcode || imm < memo->prec)
								goto failure;
							ir = memo->sa.index, cr = memo->sa.position.column, lr = memo->sa.position.line;
							ac = semantics_.restore_actions_after(ac, memo->actions);
							continue;
						}
						stack_frames_.push_back(stack_frame_type::lrcall);
						lrcall_stack_.push_back({ac,pc,pc+off,{ir,cr,lr},{lrfailcode,0,0},std::vector<semantic_action>{},imm});
					} else {
						stack_frames_.push_back(stack_frame_type::call);
						call_stack_.push_back({ac,pc});
					}
					ac = semantics_.push_action([](semantic_environment& s) { s.enter_frame(); });
					pc += off;
				} break;
				case opcode::ret: {
					ac = semantics_.push_action([](semantic_environment& s) { s.leave_frame(); });
					if (stack_frames_.empty())
						goto failure;
					switch (stack_frames_.back()) {
						case stack_frame_type::call: {
							pc = call_stack_.back().program_counter;
							call_stack_.pop_back();
						} break;
						case stack_frame_type::lrcall: {
							auto& memo = lrcall_stack_.back();
							if (memo.sa.index == lrfailcode || ir > memo.sa.index) {
								memo.sa = {ir, cr, lr};
								memo.actions = semantics_.drop_actions_after(memo.acr);
								load_registers(memo.sr, {memo.acr, memo.pca}, ir, cr, lr, ac, pc);
								ac = semantics_.push_action([](semantic_environment& s) { s.enter_frame(); });
								continue;
							}
							load_registers(memo.sa, {semantics_.restore_actions_after(memo.acr, memo.actions), memo.pcr}, ir, cr, lr, ac, pc);
							lrcall_stack_.pop_back();
						} break;
						default: goto failure;
					}
					stack_frames_.pop_back();
				} break;
				case opcode::fail: {
					fc = imm;
				failure:
					save_registers(ir, cr, lr, ac, pc);
					for (++fc; fc > 0; --fc) {
						if (stack_frames_.empty()) {
							done = true;
							break;
						}
						stack_frame_type type = stack_frames_.back();
						stack_frames_.pop_back();
						switch (type) {
							case stack_frame_type::backtrack: {
								const auto& frame = backtrack_stack_.back();
								program_state_ = std::get<0>(frame);
								input_state_ = std::get<1>(frame);
								backtrack_stack_.pop_back();
							} break;
							case stack_frame_type::call: {
								call_stack_.pop_back(), ++fc;
							} break;
							case stack_frame_type::lrcall: {
								auto& memo = lrcall_stack_.back();
								if (memo.sa.index != lrfailcode) {
									program_state_ = {semantics_.restore_actions_after(memo.acr, memo.actions), memo.pcr};
									input_state_ = memo.sa;
								} else { ++fc; }
								lrcall_stack_.pop_back();
							} break;
							case stack_frame_type::capture: {
								capture_stack_.pop_back(), ++fc;
							} break;
							default: break;
						}
					}
					load_registers(ir, cr, lr, ac, pc);
				} break;
				case opcode::accept: {
					save_registers(ir, cr, lr, ac, pc);
					semantics_.accept(input_);
					if (result = done = imm != 0; done)
						break;
					load_registers(ir, cr, lr, ac, pc);
				} break;
				case opcode::newline: {
					cr = 1, lr += 1;
				} break;
				case opcode::predicate: {
					save_registers(ir, cr, lr, ac, pc);
					if (!prog.predicates[imm](*this))
						goto failure;
					load_registers(ir, cr, lr, ac, pc);
				} break;
				case opcode::action: {
					ac = semantics_.push_action(prog.actions[imm]);
				} break;
				case opcode::begin_capture: {
					stack_frames_.push_back(stack_frame_type::capture);
					capture_stack_.push_back({ir, cr, lr});
				} break;
				case opcode::end_capture: {
					if (stack_frames_.empty() || stack_frames_.back() != stack_frame_type::capture)
						goto failure;
					auto last = ir;
					auto end = syntax_position{cr, lr};
					auto [first, start] = capture_stack_.back();
					capture_stack_.pop_back(), stack_frames_.pop_back();
					if (first > last)
						goto failure;
					ac = semantics_.push_action([a = prog.syntax_actions[imm], first, last, start, end](semantic_environment& s) {
						a(s, {s.capture().substr(first, last - first), start, end}); });
				} break;
				default: throw parser_error{"invalid opcode"};
			}
		}
		return result;
	}
};

template <class InputIt, class = typename std::enable_if<std::is_same<char, typename std::iterator_traits<InputIt>::value_type>::value>::type>
inline bool parse(InputIt first, InputIt last, const grammar& grmr, semantic_environment& sema) {
	return parser{grmr, sema}.enqueue(first, last).parse();
}

template <class InputIt, class = typename std::enable_if<std::is_same<char, typename std::iterator_traits<InputIt>::value_type>::value>::type>
inline bool parse(InputIt first, InputIt last, const grammar& grmr) {
	semantic_environment sema;
	return parse(first, last, grmr, sema);
}

inline bool parse(std::istream& input, const grammar& grmr, semantic_environment& sema) {
	return parser{grmr, sema}.push_source([&input](std::string& line) {
		if (std::getline(input, line)) {
			line.push_back('\n');
			return true;
		}
		return false;
	}).parse();
}

inline bool parse(std::istream& input, const grammar& grmr) {
	semantic_environment sema;
	return parse(input, grmr, sema);
}

inline bool parse(const std::string& input, const grammar& grmr, semantic_environment& sema) { return parse(input.cbegin(), input.cend(), grmr, sema); }
inline bool parse(const std::string& input, const grammar& grmr) { return parse(input.cbegin(), input.cend(), grmr); }
inline bool parse(const grammar& grmr, semantic_environment& sema) { return parse(std::cin, grmr, sema); }
inline bool parse(const grammar& grmr) { return parse(std::cin, grmr); }

inline grammar string_expression::make_grammar() {
	using namespace language;
	constexpr auto A = any_terminal{};
	using C = char_terminal;
	rule Empty = empty_terminal{}                               <[](generator& g) { g.encoder.encode(opcode::match); };
	rule Dot = C{'.'}                                           <[](generator& g) { g.encoder.encode(opcode::match_any); };
	rule Element = A > C{'-'} > !C{']'} > A                     <[](generator& g, syntax x) { g.bracket_range(x.capture); }
		| C{'['} > C{':'} > +(!C{':'} > A) > C{':'} > C{']'}    <[](generator& g, syntax x) { g.bracket_class(x.capture.substr(2, x.capture.size() - 4)); }
		| A                                                     <[](generator& g, syntax x) { g.bracket_range(x.capture, x.capture); };
	rule Bracket = C{'['} > ~(C{'^'}                            <[](generator& g) { g.circumflex = true; })
		> Element > *(!C{']'} > Element) > C{']'}               <[](generator& g) { g.bracket_commit(); };
	rule Sequence = +(!(C{'.'} | C{'['}) > A)                   <[](generator& g, syntax x) { g.encoder.match(x.capture); };
	return start((+(Dot | Bracket | Sequence) | Empty) > !A);
}

inline void string_expression::compile(const std::string& s) {
	static grammar grmr = make_grammar();
	generator genr(*this);
	if (!parse(s, grmr, genr))
		throw grammar_error{"invalid string or bracket expression"};
}

} // namespace lug

#endif // LUG_HPP
