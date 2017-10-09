// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017 Jesse W. Towner
// See LICENSE.md file for license details

#ifndef LUG_HPP__
#define LUG_HPP__

#include <lug/unicode.hpp>
#include <lug/utf8.hpp>
#include <any>
#include <functional>
#include <iostream>
#include <locale>
#include <memory>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace lug
{

struct program; class rule; class grammar; class encoder; class rule_encoder; class parser; class semantics;
class lug_error : public std::runtime_error { using std::runtime_error::runtime_error; };
class program_limit_error : public lug_error { public: program_limit_error() : lug_error{"length or offset of program exceeds internal limit"} {} };
class resource_limit_error : public lug_error { public: resource_limit_error() : lug_error{"number of resources exceeds internal limit"} {} };
class reenterant_parse_error : public lug_error { public: reenterant_parse_error() : lug_error{"parsing is non-reenterant"} {} };
class reenterant_read_error : public lug_error { public: reenterant_read_error() : lug_error{"attempted to read or modify input source while reading"} {} };
class bad_string_expression : public lug_error { public: using lug_error::lug_error; bad_string_expression() : lug_error{"invalid string or bracket expression"} {} };
class bad_character_class : public bad_string_expression { public: bad_character_class() : bad_string_expression{"invalid character class"} {} };
class bad_grammar : public lug_error { public: bad_grammar() : lug_error{"invalid or empty grammar"} {} };
class bad_opcode : public lug_error { public: bad_opcode() : lug_error{"invalid opcode"} {} };
struct syntax_position { std::size_t column, line; };
struct syntax_range { std::size_t index, size; syntax_position start, end; };
struct syntax_view { std::string_view capture; syntax_position start, end; };
struct prospective_action { unsigned short call_depth, action_index; unsigned int capture_index; };
typedef std::function<bool(parser&)> semantic_predicate;
typedef std::function<void(semantics&)> semantic_action;
typedef std::function<void(semantics&, const syntax_view&)> semantic_capture;
template <class E> constexpr bool is_callable_v = std::is_same_v<grammar, std::decay_t<E>> || std::is_same_v<rule, std::decay_t<E>> || std::is_same_v<program, std::decay_t<E>>;
template <class E> constexpr bool is_predicate_v = std::is_invocable_r_v<bool, E, parser&> || std::is_invocable_r_v<bool, E>;
template <class E> constexpr bool is_proper_expression_v = std::is_invocable_v<E, encoder&>;
template <class E> constexpr bool is_string_expression_v = std::is_convertible_v<E, std::string>;
template <class E> constexpr bool is_expression_v = is_callable_v<E> || is_predicate_v<E> || is_proper_expression_v<E> || is_string_expression_v<E>;

namespace detail
{

template <class Error, class T, class U, class V> inline void assure_in_range(T x, U minval, V maxval) { if (!(minval <= x && x <= maxval)) throw Error{}; }
template <class Error, class T, class U> inline auto checked_add(T x, U y) { if ((std::numeric_limits<decltype(x + y)>::max)() - x < y) throw Error{}; return x + y; }

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
	reentrancy_sentinel(bool& x) : value{x} { if (value) throw Error{}; value = true; }
	~reentrancy_sentinel() { value = false; }
};

} // namespace detail

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
	instruction(std::ptrdiff_t o) : off{static_cast<int>(o)} { if (off != o) throw program_limit_error{}; }
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
			len += static_cast<std::ptrdiff_t>(((p.val & 0xff) >> 2) + 1);
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
	std::vector<semantic_capture> captures;

	void concatenate(const program& src) {
		instructions.reserve(detail::checked_add<program_limit_error>(instructions.size(), src.instructions.size()));
		for (auto i = src.instructions.begin(), j = i, e = src.instructions.end(); i != e; i = j) {
			instruction instr = *i;
			std::size_t valoffset;
			switch (instr.pf.op) {
				case opcode::predicate: valoffset = predicates.size(); break;
				case opcode::action: valoffset = actions.size(); break;
				case opcode::end_capture: valoffset = captures.size(); break;
				default: valoffset = 0; break;
			}
			if (valoffset != 0) {
				std::size_t val = instr.pf.val + valoffset;
				detail::assure_in_range<resource_limit_error>(val, valoffset, (std::numeric_limits<unsigned short>::max)());
				instr.pf.val = static_cast<unsigned short>(val);
			}
			j = std::next(i, instruction::length(instr.pf));
			instructions.push_back(instr);
			instructions.insert(instructions.end(), i + 1, j);
		}
		predicates.insert(predicates.end(), src.predicates.begin(), src.predicates.end());
		actions.insert(actions.end(), src.actions.begin(), src.actions.end());
		captures.insert(captures.end(), src.captures.begin(), src.captures.end());
	}

	void swap(program& p) {
		instructions.swap(p.instructions);
		predicates.swap(p.predicates);
		actions.swap(p.actions);
		captures.swap(p.captures);
	}
};

class rule
{
	friend class encoder; friend class rule_encoder;
	friend grammar start(const rule&);
	bool encoding_{false};
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
	auto operator[](unsigned short precedence) const noexcept;
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

class semantics
{
	friend class parser;
	std::string_view match_;
	unsigned short prune_depth_, call_depth_;
	std::vector<prospective_action> prospective_actions_;
	std::vector<syntax_range> captures_;
	std::vector<std::any> attributes_;
	virtual void on_accept_begin() {}
	virtual void on_accept_end() {}
	virtual void on_clear() {}
	std::size_t action_count() const noexcept { return prospective_actions_.size(); }
	void pop_actions_after(std::size_t n) { if (n < prospective_actions_.size()) prospective_actions_.resize(n); }

	auto drop_actions_after(std::size_t n) {
		std::vector<prospective_action> dropped;
		if (n < prospective_actions_.size()) {
			dropped.assign(prospective_actions_.begin() + n, prospective_actions_.end());
			prospective_actions_.resize(n);
		}
		return dropped;
	}

	auto restore_actions_after(std::size_t n, const std::vector<prospective_action>& a) {
		pop_actions_after(n);
		prospective_actions_.insert(prospective_actions_.end(), a.begin(), a.end());
		return action_count();
	}

	auto push_action(std::size_t depth, std::size_t action_index, unsigned int capture_index = (std::numeric_limits<unsigned int>::max)()) {
		prospective_actions_.push_back({static_cast<unsigned short>(depth), static_cast<unsigned short>(action_index), capture_index});
		return action_count();
	}

	auto push_capture_action(std::size_t depth, std::size_t action_index, const syntax_range& range) {
		captures_.push_back(range);
		return push_action(depth, action_index, static_cast<unsigned int>(captures_.size() - 1));
	}

public:
	virtual ~semantics() = default;
	const std::string_view& match() const { return match_; }
	void escape() { prune_depth_ = call_depth_; }
	unsigned short call_depth() const { return call_depth_; }
	template <class T> void push_attribute(T&& x) { attributes_.emplace_back(std::in_place_type<T>, ::std::forward<T>(x)); }
	template <class T, class... Args> void push_attribute(Args&&... args) { attributes_.emplace_back(std::in_place_type<T>, ::std::forward<Args>(args)...); }
	template <class T> T pop_attribute() { T r{::std::any_cast<T>(attributes_.back())}; attributes_.pop_back(); return r; }

	void accept(const lug::grammar& grmr, std::string_view m) {
		const auto& actions = grmr.program().actions;
		const auto& captures = grmr.program().captures;
		match_ = m;
		on_accept_begin();
		for (auto& pa : prospective_actions_) {
			if (prune_depth_ <= pa.call_depth)
				continue;
			prune_depth_ = (std::numeric_limits<unsigned short>::max)(), call_depth_ = pa.call_depth;
			if (pa.capture_index < (std::numeric_limits<unsigned int>::max)()) {
				const syntax_range& cap = captures_[pa.capture_index];
				captures[pa.action_index](*this, {match_.substr(cap.index, cap.size), cap.start, cap.end});
			} else {
				actions[pa.action_index](*this);
			}
		}
		on_accept_end();
		clear();
	}

	void clear() {
		match_ = std::string_view{}, prune_depth_ = (std::numeric_limits<unsigned short>::max)(), call_depth_ = 0;
		prospective_actions_.clear(), attributes_.clear();
		on_clear();
	}
};

template <class T>
class variable
{
	semantics& semantics_;
	std::unordered_map<unsigned short, T> state_;
public:
	variable(semantics& s) : semantics_{s} {}
	T* operator->() { return &state_[semantics_.call_depth()]; }
	const T* operator->() const { return &state_[semantics_.call_depth()]; }
	T& operator*() { return state_[semantics_.call_depth()]; }
	const T& operator*() const { return state_[semantics_.call_depth()]; }
};

class encoder
{
	virtual void do_append(instruction) = 0;
	virtual void do_append(const program&) = 0;
	virtual immediate do_add_semantic_predicate(semantic_predicate) { return immediate{0}; }
	virtual immediate do_add_semantic_action(semantic_action) { return immediate{0}; }
	virtual immediate do_add_semantic_capture_action(semantic_capture) { return immediate{0}; }
	virtual void do_add_callee(const rule*, const program*, std::ptrdiff_t) {}
	virtual bool do_should_evaluate_length() const noexcept { return true; }
	virtual std::ptrdiff_t do_length() const noexcept = 0;
public:
	virtual ~encoder() = default;
	encoder& append(instruction instr) { do_append(instr); return *this; }
	encoder& append(const program& p) { do_append(p); return *this; }
	template <class InputIt> encoder& append(InputIt first, InputIt last) { for ( ; first != last; ++first) do_append(*first); return *this; }
	encoder& call(const program& p, unsigned short prec) { do_add_callee(nullptr, &p, length()); return encode(opcode::call, 0, immediate{prec}); }
	encoder& call(const grammar& g, unsigned short prec) { do_add_callee(nullptr, &g.program(), length()); return encode(opcode::call, 3, immediate{prec}); }
	encoder& encode(opcode op, immediate imm = immediate{0}) { return append(instruction{op, operands::none, imm}); }
	encoder& encode(opcode op, semantic_predicate p) { return append(instruction{op, operands::none, do_add_semantic_predicate(std::move(p))}); }
	encoder& encode(opcode op, semantic_action a) { return append(instruction{op, operands::none, do_add_semantic_action(std::move(a))}); }
	encoder& encode(opcode op, semantic_capture a) { return append(instruction{op, operands::none, do_add_semantic_capture_action(std::move(a))}); }
	encoder& encode(opcode op, std::ptrdiff_t off, immediate imm = immediate{0}) { return append(instruction{op, operands::off, imm}).append(instruction{off}); }
	template <class E> auto evaluate(const E& e) -> std::enable_if_t<is_expression_v<E>, encoder&>;
	template <class E> auto evaluate_length(const E& e) -> std::enable_if_t<is_expression_v<E>, std::ptrdiff_t>;
	std::ptrdiff_t length() const noexcept { return do_length(); }

	encoder& call(const rule& r, unsigned short prec) {
		if (auto& p = r.program_; prec <= 0 && !r.encoding_ && r.callees_.empty() && !p.instructions.empty() && p.instructions.size() <= 8 &&
				p.predicates.size() <= 1 && p.actions.size() <= 1 && p.captures.size() <= 1) {
			return append(r.program_);
		} else {
			do_add_callee(&r, &r.program_, length());
			return encode(opcode::call, 0, immediate{prec});
		}
	}

	encoder& encode(opcode op, std::size_t val, std::string_view subsequence) {
		if (!subsequence.empty()) {
			detail::assure_in_range<resource_limit_error>(val, 1u, instruction::maxstrlen);
			detail::assure_in_range<resource_limit_error>(subsequence.size(), 1u, instruction::maxstrlen);
			do_append(instruction{op, operands::str, static_cast<immediate>(((val - 1) << 8) | (subsequence.size() - 1))});
			do {
				do_append(instruction{subsequence});
				subsequence.remove_prefix((std::min)(std::size_t{4}, subsequence.size()));
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
		return first == last ? match(first) : encode(opcode::match_range, first.size(), std::string{first}.append(last));
	}
};

class instruction_length_evaluator : public encoder
{
	std::ptrdiff_t length_{0};
	void do_append(instruction) override { length_ = detail::checked_add<program_limit_error>(length_, std::ptrdiff_t{1}); }
	void do_append(const program& p) override { length_ = detail::checked_add<program_limit_error>(length_, static_cast<std::ptrdiff_t>(p.instructions.size())); }
	bool do_should_evaluate_length() const noexcept override { return false; }
	std::ptrdiff_t do_length() const noexcept override { return length_; }
};

class instruction_encoder : public encoder
{
	std::vector<instruction>& instructions_;
	std::ptrdiff_t do_length() const noexcept override { return static_cast<std::ptrdiff_t>(instructions_.size()); }
	void do_append(instruction instr) override { instructions_.push_back(instr); }
	void do_append(const program&) override { throw bad_grammar{}; }
public:
	explicit instruction_encoder(std::vector<instruction>& i) : instructions_{i} {}
};

class program_encoder : public instruction_encoder
{
	program& program_;
	void do_append(const program& p) override { program_.concatenate(p); }
	immediate do_add_semantic_predicate(semantic_predicate p) override { return add_item(program_.predicates, std::move(p)); }
	immediate do_add_semantic_action(semantic_action a) override { return add_item(program_.actions, std::move(a)); }
	immediate do_add_semantic_capture_action(semantic_capture a) override { return add_item(program_.captures, std::move(a)); }

	template <class Item>
	immediate add_item(std::vector<Item>& items, Item&& item) {
		detail::assure_in_range<resource_limit_error>(items.size(), 0u, (std::numeric_limits<unsigned short>::max)() - 1u);
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
	explicit rule_encoder(rule& r) : program_encoder{r.program_}, rule_{r} { rule_.encoding_ = true; }
	~rule_encoder() override { rule_.encoding_ = false; }
};

class string_expression
{
	std::vector<instruction> instructions_;
	static grammar make_grammar();
	void compile(std::string_view sv);

	struct generator : semantics
	{
		instruction_encoder encoder;
		bool circumflex;
		std::ctype_base::mask classes;
		std::vector<std::pair<std::string_view, std::string_view>> ranges;
		generator(string_expression& se) : encoder{se.instructions_}, circumflex{false}, classes{0} {}

		void bracket_class(std::string_view s) {
			using ct = std::ctype_base; static constexpr std::pair<const char* const, int> m[12] = {
				{"alnum", ct::alnum}, {"alpha", ct::alpha}, {"blank", ct::blank}, {"cntrl", ct::cntrl},
				{"digit", ct::digit}, {"graph", ct::graph}, {"lower", ct::lower}, {"print", ct::print},
				{"punct", ct::punct}, {"space", ct::space}, {"upper", ct::upper}, {"xdigit", ct::xdigit}};
			auto c = std::find_if(std::begin(m), std::end(m), [s](auto& x) { return !s.compare(x.first); });
			if (c == std::end(m)) throw bad_character_class{};
			classes |= static_cast<std::ctype_base::mask>(c->second);
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
				for (auto curr = merged.end(), next = ranges.begin(), last = ranges.end(); next != last; ++next)
					if (curr == merged.end() || next->first < curr->first || curr->second < next->first)
						curr = merged.insert(merged.end(), *next); 
					else
						curr->second = curr->second < next->second ? next->second : curr->second;
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
				encoder.encode(opcode::commit, 0).encode(opcode::fail).encode(opcode::match_any);
			circumflex = false;
			classes = 0;
			ranges.clear();
		}
	};

public:
	string_expression(std::string_view sv) { compile(sv); }
	void operator()(encoder& d) const { d.append(instructions_.cbegin(), instructions_.cend()); }
};

template <class T>
constexpr auto make_expression(const T& t) {
	static_assert(is_expression_v<T>, "T must be an expression type");
	if constexpr (is_callable_v<T>)
		return [&target = t](encoder& d){ d.call(target, 0); };
	else if constexpr (std::is_invocable_r_v<bool, T, parser&>)
		return [p = semantic_predicate{t}](encoder& d) { d.encode(opcode::predicate, p); };
	else if constexpr (std::is_invocable_r_v<bool, T>)
		return [p = semantic_predicate{[a = T{t}](parser&){ return a(); }}](encoder& d) { d.encode(opcode::predicate, p); };
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

template <class E>
inline auto encoder::evaluate_length(const E& e) -> std::enable_if_t<is_expression_v<E>, std::ptrdiff_t> {
	return do_should_evaluate_length() ? instruction_length_evaluator{}.evaluate(e).length() : 0;
}

template <class E, class> inline rule::rule(const E& e) { rule_encoder{*this}.evaluate(e); }
inline rule::rule(const rule& r) { rule_encoder{*this}.call(r, 1); }
inline auto rule::operator[](unsigned short precedence) const noexcept { return [&target = *this, precedence](encoder& d){ d.call(target, precedence); }; }

constexpr struct {
	auto operator()(char c) const { return [c](encoder& d){ d.match(std::string_view{&c, 1}); }; }
	auto operator()(char s, char e) const { return [s, e](encoder& d) { d.match_range(std::string_view{&s, 1}, std::string_view{&e, 1}); }; }
} chr = {};

constexpr struct { void operator()(encoder& d) const { d.encode(opcode::match_any); } } any = {};
constexpr struct { void operator()(encoder& d) const { d.encode(opcode::accept); } } cut = {};
constexpr struct { void operator()(encoder& d) const { d.encode(opcode::newline); } } ilr = {};
constexpr struct { void operator()(encoder& d) const { d.encode(opcode::match); } } eps = {};
constexpr struct { void operator()(encoder& d) const { d.encode(opcode::choice, 2).encode(opcode::match_any).encode(opcode::fail, immediate{1}); } } eoi = {};

constexpr struct { void operator()(encoder& d) const {
	d.encode(opcode::choice, 4).match("\n").encode(opcode::commit, 7).match("\r")
	 .encode(opcode::choice, 3).match("\n").encode(opcode::commit).encode(opcode::newline); } } eol = {};

constexpr struct { void operator()(encoder& d) const {
	static const size_t eol_length = instruction_length_evaluator{}.evaluate(eol).length();
	d.encode(opcode::choice, 4).match(" ").encode(opcode::commit, 6 + eol_length)
	 .encode(opcode::choice, 2 + eol_length).evaluate(eol).encode(opcode::commit, 2).match_range("\t", "\r"); } } space = {};

namespace language
{

using parser = lug::parser; using semantics = lug::semantics; using syntax = const lug::syntax_view&;
template <class T> using variable = lug::variable<T>;
using lug::grammar; using lug::rule; using lug::start;
using namespace std::literals::string_literals;

template <class E, class = std::enable_if_t<is_expression_v<E>>>
constexpr auto operator!(const E& e) {
	return [x = make_expression(e)](encoder& d) { d.encode(opcode::choice, 1 + d.evaluate_length(x)).evaluate(x).encode(opcode::fail, immediate{1}); };
}

template <class E, class = std::enable_if_t<is_expression_v<E>>>
constexpr auto operator*(const E& e) {
	return [x = make_expression(e)](encoder& d) { auto n = d.evaluate_length(x); d.encode(opcode::choice, 2 + n).evaluate(x).encode(opcode::commit, -(4 + n)); };
}

template <class E1, class E2, class = std::enable_if_t<is_expression_v<E1> && is_expression_v<E2>>>
constexpr auto operator|(const E1& e1, const E2& e2) {
	return [x1 = make_expression(e1), x2 = make_expression(e2)](encoder& d) {
		d.encode(opcode::choice, 2 + d.evaluate_length(x1)).evaluate(x1).encode(opcode::commit, d.evaluate_length(x2)).evaluate(x2); };
}

template <class E1, class E2, class = std::enable_if_t<is_expression_v<E1> && is_expression_v<E2>>>
constexpr auto operator>(const E1& e1, const E2& e2) {
	return [e1 = make_expression(e1), e2 = make_expression(e2)](encoder& d) { d.evaluate(e1).evaluate(e2); };
}

template <class E, class A, class = std::enable_if_t<is_expression_v<E>>>
constexpr auto operator<(const E& e, A a) {
	if constexpr (std::is_invocable_v<A, semantics&, syntax>) {
		return [e = make_expression(e), a = ::std::move(a)](encoder& d) { d.encode(opcode::begin_capture).evaluate(e).encode(opcode::end_capture, semantic_capture{a}); };
	} else if constexpr (std::is_invocable_v<A, detail::dynamic_cast_if_base_of<semantics&>, syntax>) {
		return e <[a = std::move(a)](semantics& s, syntax x) { a(detail::dynamic_cast_if_base_of<semantics&>{s}, x); };
	} else if constexpr (std::is_invocable_v<A, semantics&>) {
		return [e = make_expression(e), a = ::std::move(a)](encoder& d) { d.evaluate(e).encode(opcode::action, semantic_action{a}); };
	} else if constexpr (std::is_invocable_v<A, detail::dynamic_cast_if_base_of<semantics&>>) {
		return e <[a = std::move(a)](semantics& s) { a(detail::dynamic_cast_if_base_of<semantics&>{s}); };
	} else if constexpr (std::is_invocable_v<A> && std::is_same_v<void, std::invoke_result_t<A>>) {
		return [e = make_expression(e), a = ::std::move(a)](encoder& d) { d.evaluate(e).encode(opcode::action, [a](semantics&) { a(); }); };
	} else if constexpr (std::is_invocable_v<A>) {
		return [e = make_expression(e), a = ::std::move(a)](encoder& d) { d.evaluate(e).encode(opcode::action, [a](semantics& s) { s.push_attribute(a()); }); };
	}
}

template <class T, class E, class = std::enable_if_t<is_expression_v<E>>>
inline auto operator<<(variable<T>& v, const E& e) { return e < [&v](semantics&, syntax x) { *v = T{x.capture}; }; }
template <class T, class E, class = std::enable_if_t<is_expression_v<E>>>
inline auto operator%(variable<T>& v, const E& e) { return e < [&v](semantics& s) { *v = s.pop_attribute<T>(); }; }
template <class E, class = std::enable_if_t<is_expression_v<E>>> constexpr auto operator&(const E& e) { return !(!e); }
template <class E, class = std::enable_if_t<is_expression_v<E>>> constexpr auto operator+(const E& e) { auto x = make_expression(e); return x > *x; }
template <class E, class = std::enable_if_t<is_expression_v<E>>> constexpr auto operator~(const E& e) { return e | eps; }
template <class E, class = std::enable_if_t<is_expression_v<E>>> constexpr auto operator--(const E& e) { return cut > e; }
template <class E, class = std::enable_if_t<is_expression_v<E>>> constexpr auto operator--(const E& e, int) { return e > cut; }

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
		detail::assure_in_range<program_limit_error>(reladdr, std::numeric_limits<int>::lowest(), (std::numeric_limits<int>::max)());
		ioffset.off = static_cast<int>(reladdr);
	}
	return grammar(std::move(gp));
}

class parser
{
	enum class stack_frame_type : unsigned char { backtrack, call, lrcall, capture };
	struct syntax_state { std::size_t index; syntax_position position; };
	struct program_state { std::size_t action_counter; std::ptrdiff_t program_counter; };
	struct lrmemo_state { std::size_t acr; std::ptrdiff_t pcr, pca; syntax_state sr, sa; std::vector<prospective_action> actions; std::size_t prec; };
	static constexpr std::size_t lrfailcode = (std::numeric_limits<std::size_t>::max)();

	const lug::grammar& grammar_;
	lug::semantics& semantics_;
	std::locale locale_;
	std::string input_;
	syntax_state input_state_{0, 1, 1}, max_input_state_{0, 1, 1};
	program_state program_state_{0, 0};
	bool parsing_{false}, reading_{false}, cut_deferred_{false};
	std::size_t cut_frame_{0};
	std::vector<std::function<bool(std::string&)>> sources_;
	std::vector<stack_frame_type> stack_frames_;
	std::vector<std::pair<program_state, syntax_state>> backtrack_stack_;
	std::vector<program_state> call_stack_;
	std::vector<syntax_state> capture_stack_;
	std::vector<lrmemo_state> lrmemo_stack_;

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
		detail::reentrancy_sentinel<reenterant_read_error> guard{reading_};
		std::string text;
		while (!sources_.empty() && text.empty()) {
			bool more = sources_.back()(text);
			input_.insert(input_.end(), text.begin(), text.end());
			if (!more)
				sources_.pop_back();
		}
		return !text.empty();
	}

	void cut() {
		semantics_.accept(grammar_, input_);
		input_.erase(0, input_state_.index);
		input_state_.index = 0, max_input_state_.index = 0, program_state_.action_counter = 0;
		cut_deferred_ = false, cut_frame_ = stack_frames_.size();
	}

	void cut(std::size_t& ir, std::size_t& cr, std::size_t& lr, std::size_t& ac, std::ptrdiff_t& pc) {
		save_registers(ir, cr, lr, ac, pc); cut(); load_registers(ir, cr, lr, ac, pc);
	}

	template <class Stack, class... Args>
	void pop_stack_frame(Stack& stack, Args&... args) {
		stack.pop_back(), stack_frames_.pop_back();
		cut_frame_ = (std::min)(cut_frame_, stack_frames_.size());
		if constexpr (std::is_same_v<typename Stack::value_type, syntax_state> || std::is_same_v<typename Stack::value_type, lrmemo_state>) {
			if (cut_deferred_ && capture_stack_.empty() && lrmemo_stack_.empty())
				cut(args...);
		}
	}

	void load_registers(const syntax_state& ss, const program_state& ps, std::size_t& ir, std::size_t& cr, std::size_t& lr, std::size_t& ac, std::ptrdiff_t& pc) {
		ir = ss.index, cr = ss.position.column, lr = ss.position.line, ac = ps.action_counter, pc = ps.program_counter;
		semantics_.pop_actions_after(ac);
	}

public:
	parser(const lug::grammar& g, lug::semantics& s) : grammar_{g}, semantics_{s} {}
	const lug::grammar& grammar() const noexcept { return grammar_; }
	lug::semantics& semantics() const noexcept { return semantics_; }
	std::string_view input_view() const noexcept { return std::string_view{input_.data() + input_state_.index, input_.size() - input_state_.index}; }
	const syntax_position& input_position() const noexcept { return input_state_.position; }
	const syntax_position& max_input_position() const noexcept { return max_input_state_.position; }
	bool available(std::size_t n) { available(n, input_state_.index); }

	void save_registers(std::size_t ir, std::size_t cr, std::size_t lr, std::size_t ac, std::ptrdiff_t pc) {
		input_state_ = {ir, cr, lr}, program_state_ = {ac, pc};
		if (ir > max_input_state_.index)
			max_input_state_ = input_state_;
	}

	void load_registers(std::size_t& ir, std::size_t& cr, std::size_t& lr, std::size_t& ac, std::ptrdiff_t& pc) {
		load_registers(input_state_, program_state_, ir, cr, lr, ac, pc);
	}

	template <class InputIt, class = utf8::enable_if_char_input_iterator_t<InputIt>>
	parser& enqueue(InputIt first, InputIt last) {
		input_.insert(input_.end(), first, last);
		return *this;
	}

	template <class InputFunc>
	parser& push_source(InputFunc&& func) {
		if (reading_) throw reenterant_read_error{};
		sources_.emplace_back(::std::forward<InputFunc>(func));
		return *this;
	}

	bool parse() {
		detail::reentrancy_sentinel<reenterant_parse_error> guard{parsing_};
		const program& prog = grammar_.program();
		if (prog.instructions.empty()) throw bad_grammar{};
		bool result = false, done = false;
		std::size_t imm, ir, cr, lr, ac, fc = 0;
		std::ptrdiff_t off, pc;
		std::string_view str;
		semantics_.clear();
		program_state_ = {0, 0};
		cut_deferred_ = false;
		load_registers(ir, cr, lr, ac, pc);
		while (!done) {
			switch (instruction::decode(prog.instructions, pc, imm, off, str)) {
				case opcode::match: {
					if (!str.empty()) {
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
					const auto& codecvt = std::use_facet<std::codecvt<wchar_t, char, std::mbstate_t>>(locale_);
					std::wstring_convert<std::codecvt<wchar_t, char, std::mbstate_t>, wchar_t> cvt(&codecvt);
					auto wc = cvt.from_bytes(input_.data() + ir, input_.data() + ir + sz);
					if (wc.empty() || !std::use_facet<std::ctype<wchar_t>>(locale_).is(static_cast<short>(imm), wc[0]))
						goto failure;
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
					backtrack_stack_.push_back({{ac, pc + off}, {ir - (imm & 255), cr - (imm >> 8), lr}});
				} break;
				case opcode::commit: {
					if (stack_frames_.empty() || stack_frames_.back() != stack_frame_type::backtrack)
						goto failure;
					if (imm > 1) {
						backtrack_stack_.back().second = input_state_;
					} else if (imm <= 1) {
						if (imm == 1)
							input_state_ = backtrack_stack_.back().second;
						pop_stack_frame(backtrack_stack_);
					}
				} [[fallthrough]];
				case opcode::jump: {
					pc += off;
				} break;
				case opcode::call: {
					if (imm != 0) {
						if (auto memo = std::find_if(lrmemo_stack_.crbegin(), lrmemo_stack_.crend(),
									[pca = pc + off, ir](auto& x) { return pca == x.pca && ir == x.sr.index; });
								memo != lrmemo_stack_.crend()) {
							if (memo->sa.index == lrfailcode || imm < memo->prec)
								goto failure;
							ir = memo->sa.index, cr = memo->sa.position.column, lr = memo->sa.position.line;
							ac = semantics_.restore_actions_after(ac, memo->actions);
							continue;
						}
						stack_frames_.push_back(stack_frame_type::lrcall);
						lrmemo_stack_.push_back({ac, pc, pc + off, {ir, cr, lr}, {lrfailcode, 0, 0}, std::vector<prospective_action>{}, imm});
					} else {
						stack_frames_.push_back(stack_frame_type::call);
						call_stack_.push_back({ac, pc});
					}
					pc += off;
				} break;
				case opcode::ret: {
					if (stack_frames_.empty())
						goto failure;
					switch (stack_frames_.back()) {
						case stack_frame_type::call: {
							pc = call_stack_.back().program_counter;
							pop_stack_frame(call_stack_);
						} break;
						case stack_frame_type::lrcall: {
							auto& memo = lrmemo_stack_.back();
							if (memo.sa.index == lrfailcode || ir > memo.sa.index) {
								memo.sa = {ir, cr, lr};
								memo.actions = semantics_.drop_actions_after(memo.acr);
								load_registers(memo.sr, {memo.acr, memo.pca}, ir, cr, lr, ac, pc);
								continue;
							}
							load_registers(memo.sa, {semantics_.restore_actions_after(memo.acr, memo.actions), memo.pcr}, ir, cr, lr, ac, pc);
							pop_stack_frame(lrmemo_stack_, ir, cr, lr, ac, pc);
						} break;
						default: goto failure;
					}
				} break;
				case opcode::fail: {
					fc = imm;
				failure:
					save_registers(ir, cr, lr, ac, pc);
					for (++fc; fc > 0; --fc) {
						if (done = cut_frame_ >= stack_frames_.size(); done)
							break;
						stack_frame_type type = stack_frames_.back();
						switch (type) {
							case stack_frame_type::backtrack: {
								std::tie(program_state_, input_state_) = backtrack_stack_.back();
								pop_stack_frame(backtrack_stack_);
							} break;
							case stack_frame_type::call: {
								pop_stack_frame(call_stack_), ++fc;
							} break;
							case stack_frame_type::capture: {
								pop_stack_frame(capture_stack_), ++fc;
							} break;
							case stack_frame_type::lrcall: {
								auto& memo = lrmemo_stack_.back();
								if (memo.sa.index != lrfailcode) {
									program_state_ = {semantics_.restore_actions_after(memo.acr, memo.actions), memo.pcr};
									input_state_ = memo.sa;
								} else ++fc;
								pop_stack_frame(lrmemo_stack_);
							} break;
							default: break;
						}
					}
					load_registers(ir, cr, lr, ac, pc);
				} break;
				case opcode::accept: {
					if (cut_deferred_ = !capture_stack_.empty() || !lrmemo_stack_.empty(); !cut_deferred_) {
						cut(ir, cr, lr, ac, pc);
						if (result = done = imm != 0; done)
							break;
					}
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
					ac = semantics_.push_action(call_stack_.size() + lrmemo_stack_.size(), imm);
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
					pop_stack_frame(capture_stack_, ir, cr, lr, ac, pc);
					if (first > last)
						goto failure;
					ac = semantics_.push_capture_action(call_stack_.size() + lrmemo_stack_.size(), imm, syntax_range{first, last - first, start, end});
				} break;
				default: throw bad_opcode{};
			}
		}
		return result;
	}
};

template <class InputIt, class = utf8::enable_if_char_input_iterator_t<InputIt>>
inline bool parse(InputIt first, InputIt last, const grammar& grmr, semantics& sema) {
	return parser{grmr, sema}.enqueue(first, last).parse();
}

template <class InputIt, class = utf8::enable_if_char_input_iterator_t<InputIt>>
inline bool parse(InputIt first, InputIt last, const grammar& grmr) {
	semantics sema;
	return parse(first, last, grmr, sema);
}

inline bool parse(std::istream& input, const grammar& grmr, semantics& sema) {
	return parser{grmr, sema}.push_source([&input](std::string& line) {
		if (std::getline(input, line)) {
			line.push_back('\n');
			return true;
		}
		return false;
	}).parse();
}

inline bool parse(std::istream& input, const grammar& grmr) {
	semantics sema;
	return parse(input, grmr, sema);
}

inline bool parse(std::string_view sv, const grammar& grmr, semantics& sema) { return parse(sv.cbegin(), sv.cend(), grmr, sema); }
inline bool parse(std::string_view sv, const grammar& grmr) { return parse(sv.cbegin(), sv.cend(), grmr); }
inline bool parse(const grammar& grmr, semantics& sema) { return parse(std::cin, grmr, sema); }
inline bool parse(const grammar& grmr) { return parse(std::cin, grmr); }

inline grammar string_expression::make_grammar() {
	using namespace language;
	rule Empty = eps                                                            <[](generator& g) { g.encoder.encode(opcode::match); };
	rule Dot = chr('.')                                                         <[](generator& g) { g.encoder.encode(opcode::match_any); };
	rule Element = any > chr('-') > !chr(']') > any                             <[](generator& g, syntax x) { g.bracket_range(x.capture); }
		| chr('[') > chr(':') > +(!chr(':') > any) > chr(':') > chr(']')        <[](generator& g, syntax x) { g.bracket_class(x.capture.substr(2, x.capture.size() - 4)); }
		| any                                                                   <[](generator& g, syntax x) { g.bracket_range(x.capture, x.capture); };
	rule Bracket = chr('[') > ~(chr('^')                                        <[](generator& g) { g.circumflex = true; })
		> Element > *(!chr(']') > Element) > chr(']')                           <[](generator& g) { g.bracket_commit(); };
	rule Sequence = +(!(chr('.') | chr('[')) > any)                             <[](generator& g, syntax x) { g.encoder.match(x.capture); };
	return start((+(Dot | Bracket | Sequence) | Empty) > eoi);
}

inline void string_expression::compile(std::string_view sv) {
	static grammar grmr = make_grammar();
	generator genr(*this);
	if (!parse(sv, grmr, genr))
		throw bad_string_expression{};
}

} // namespace lug

#endif
