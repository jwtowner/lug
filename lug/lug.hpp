// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2024 Jesse W. Towner
// See LICENSE.md file for license details

#ifndef LUG_INCLUDE_LUG_LUG_HPP
#define LUG_INCLUDE_LUG_LUG_HPP

#include <lug/error.hpp>
#include <lug/utf8.hpp>

#include <any>
#include <iostream>
#include <numeric>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace lug {

class rule;
class grammar;
class encoder;
class rule_encoder;
class parser;
class syntax;
class environment;
struct program;
struct syntax_position { std::size_t column, line; };
struct syntax_range { std::size_t index, size; };
struct semantic_response { unsigned short call_depth, action_index; syntax_range range; };
using semantic_action = std::function<void(environment&)>;
using syntactic_capture = std::function<void(environment&, syntax const&)>;
using syntactic_predicate = std::function<bool(environment&)>;

template <class E> inline constexpr bool is_callable_v = std::is_same_v<grammar, std::decay_t<E>> || std::is_same_v<rule, std::decay_t<E>> || std::is_same_v<program, std::decay_t<E>>;
template <class E> inline constexpr bool is_predicate_v = std::is_invocable_r_v<bool, E, environment&>;
template <class E> inline constexpr bool is_proper_expression_v = std::is_invocable_v<E, encoder&>;
template <class E> inline constexpr bool is_string_expression_v = std::is_convertible_v<E, std::string>;
template <class E> inline constexpr bool is_expression_v = is_callable_v<E> || is_predicate_v<E> || is_proper_expression_v<E> || is_string_expression_v<E>;

[[nodiscard]] grammar start(rule const& start_rule);

enum class opcode : unsigned char
{
	match,          match_casefold, match_any,      match_any_of,
	match_all_of,   match_none_of,  match_set,      match_eol,
	choice,         commit,         commit_back,    commit_partial,
	jump,           call,           ret,            fail,
	accept,         accept_final,   action,         predicate,
	capture_start,  capture_end,    condition_test, condition_push,
	condition_pop,  symbol_start,   symbol_end,     symbol_exists,
	symbol_all,     symbol_any,     symbol_head,    symbol_tail,
	symbol_push,    symbol_pop
};

enum class immediate : unsigned short {};
enum class operands : unsigned char { none = 0, off = 0x40, str = 0x80, is_bitfield_enum };

union instruction
{
	static inline constexpr std::size_t maxstrlen = 256;
	struct prefix { opcode op; operands aux; unsigned short val; } pf;
	int off;
	std::array<char, 4> str;

	instruction(opcode op, operands aux, immediate imm) noexcept : pf{op, aux, static_cast<unsigned short>(imm)} {}
	explicit instruction(std::ptrdiff_t o) : off{static_cast<int>(o)} { if (off != o) throw program_limit_error{}; }
	explicit instruction(std::string_view s) { std::fill(std::copy_n(s.begin(), (std::min)(s.size(), std::size_t{4}), str.begin()), str.end(), char{0}); }

	[[nodiscard]] static auto decode(std::vector<instruction> const& code, std::ptrdiff_t& pc)
	{
		const prefix pf = code[static_cast<std::size_t>(pc++)].pf;
		const int off = ((pf.aux & operands::off) != operands::none) ? code[static_cast<std::size_t>(pc++)].off : 0;
		unsigned short imm = pf.val;
		std::string_view str;
		if ((pf.aux & operands::str) != operands::none) {
			str = std::string_view{code[static_cast<std::size_t>(pc)].str.data(), static_cast<unsigned int>((imm & 0xff) + 1)};
			pc += ((imm & 0xff) + 4) >> 2;
			imm >>= 8;
		}
		return std::make_tuple(pf.op, imm, off, str);
	}

	[[nodiscard]] static std::ptrdiff_t length(prefix pf) noexcept
	{
		std::ptrdiff_t len = 1;
		len += ((pf.aux & operands::off) != operands::none) ? 1 : 0;
		len += ((pf.aux & operands::str) != operands::none) ? static_cast<std::ptrdiff_t>(((pf.val & 0xff) >> 2) + 1) : 0;
		return len;
	}
};

static_assert(sizeof(unicode::ctype) <= sizeof(immediate), "immediate must be large enough to hold unicode::ctype");
static_assert(sizeof(unicode::sctype) <= sizeof(immediate), "immediate must be large enough to hold unicode::sctype");
static_assert(sizeof(instruction) == sizeof(int), "expected instruction to be same size as int");
static_assert(sizeof(int) <= sizeof(std::ptrdiff_t), "expected int to be no larger than ptrdiff_t");

enum class directives : unsigned int { none = 0, caseless = 1, eps = 2, lexeme = 4, noskip = 8, preskip = 16, postskip = 32, is_bitfield_enum };
using program_callees = std::vector<std::tuple<lug::rule const*, lug::program const*, std::ptrdiff_t, directives>>;

struct program
{
	std::vector<instruction> instructions;
	std::vector<unicode::rune_set> runesets;
	std::vector<semantic_action> actions;
	std::vector<syntactic_capture> captures;
	std::vector<syntactic_predicate> predicates;
	directives mandate{directives::eps};

	void concatenate(program const& src)
	{
		instructions.reserve(detail::checked_add<program_limit_error>(instructions.size(), src.instructions.size()));
		for (auto i = src.instructions.begin(), j = i, e = src.instructions.end(); i != e; i = j) {
			instruction instr = *i;
			std::size_t val;
			switch (instr.pf.op) {
				case opcode::match_set: val = detail::push_back_unique(runesets, src.runesets[instr.pf.val]); break;
				case opcode::action: val = actions.size(); actions.push_back(src.actions[instr.pf.val]); break;
				case opcode::predicate: val = predicates.size(); predicates.push_back(src.predicates[instr.pf.val]); break;
				case opcode::capture_end: val = captures.size(); captures.push_back(src.captures[instr.pf.val]); break;
				default: val = (std::numeric_limits<std::size_t>::max)(); break;
			}
			if (val != (std::numeric_limits<std::size_t>::max)()) {
				detail::assure_in_range<resource_limit_error>(val, 0u, (std::numeric_limits<unsigned short>::max)());
				instr.pf.val = static_cast<unsigned short>(val);
			}
			j = std::next(i, instruction::length(instr.pf));
			instructions.push_back(instr);
			instructions.insert(instructions.end(), i + 1, j);
		}
		mandate = (mandate & ~directives::eps) | (mandate & src.mandate & directives::eps);
	}

	void swap(program& p) noexcept
	{
		instructions.swap(p.instructions);
		runesets.swap(p.runesets);
		actions.swap(p.actions);
		captures.swap(p.captures);
		predicates.swap(p.predicates);
		std::swap(mandate, p.mandate);
	}
};

class rule
{
	friend class encoder;
	friend class rule_encoder;
	friend grammar start(rule const&);
	program program_;
	program_callees callees_;
	bool currently_encoding_{false};
public:
	rule() = default;
	template <class E, class = std::enable_if_t<is_expression_v<E>>> rule(E const& e);
	rule(rule const& r);
	rule(rule&& r) = default;
	rule& operator=(rule const& r) { rule{r}.swap(*this); return *this; }
	rule& operator=(rule&& r) = default;
	void swap(rule& r) noexcept { program_.swap(r.program_); callees_.swap(r.callees_); }
	[[nodiscard]] auto operator[](unsigned short precedence) const noexcept;
};

class grammar
{
	friend grammar start(rule const&);
	lug::program program_;
	grammar(lug::program p) : program_{std::move(p)} {}
public:
	grammar() = default;
	void swap(grammar& g) noexcept { program_.swap(g.program_); }
	[[nodiscard]] lug::program const& program() const noexcept { return program_; }
	static thread_local std::function<void(encoder&)> implicit_space;
};

class syntax
{
	std::string_view capture_;
	std::size_t index_{0};
public:
	constexpr syntax() noexcept = default;
	constexpr syntax(std::string_view c, std::size_t i) noexcept : capture_{c}, index_{i} {}
	[[nodiscard]] constexpr std::string_view capture() const noexcept { return capture_; }
	[[nodiscard]] constexpr syntax_range range() const noexcept { return syntax_range{index_, capture_.size()}; }
	[[nodiscard]] constexpr operator std::string_view() const noexcept { return capture_; }
	[[nodiscard]] constexpr operator syntax_range() const noexcept { return range(); }
	[[nodiscard]] constexpr bool empty() const noexcept { return capture_.empty(); }
	[[nodiscard]] constexpr std::size_t size() const noexcept { return capture_.size(); }
	[[nodiscard]] constexpr bool operator==(syntax const& other) const noexcept { return capture_ == other.capture_ && index_ == other.index_; }
	[[nodiscard]] constexpr bool operator!=(syntax const& other) const noexcept { return capture_ != other.capture_ || index_ != other.index_; }
};

class environment
{
	friend class lug::parser;

	std::vector<lug::parser*> parser_stack_;
	std::vector<std::vector<std::any>> accept_stack_;
	std::unordered_set<std::string_view> conditions_;
	std::unordered_map<std::string_view, std::vector<std::string>> symbols_;
	static inline const std::vector<std::string> empty_symbols_{};
	unsigned int tab_width_ = 8;
	unsigned int tab_alignment_ = 8;

	virtual void on_accept_started() {}
	virtual void on_accept_ended() {}

	void start_parse(lug::parser& p)
	{
		if (parser_stack_.size() >= (std::numeric_limits<unsigned short>::max)())
			throw resource_limit_error{};
		parser_stack_.emplace_back(&p);
	}

	void end_parse()
	{
		if (parser_stack_.empty())
			throw parse_context_error{};
		parser_stack_.pop_back();
	}

	void start_accept()
	{
		if (accept_stack_.size() >= (std::numeric_limits<unsigned short>::max)())
			throw resource_limit_error{};
		accept_stack_.emplace_back();
		on_accept_started();
	}

	void end_accept()
	{
		if (accept_stack_.empty())
			throw accept_context_error{};
		on_accept_ended();
		accept_stack_.pop_back();
	}

	[[nodiscard]] std::vector<std::any>& current_accept_context()
	{
		if (accept_stack_.empty())
			throw accept_context_error{};
		return accept_stack_.back();
	}

public:
	virtual ~environment() = default;
	[[nodiscard]] lug::parser& parser() { if (parser_stack_.empty()) throw parse_context_error{}; return *(parser_stack_.back()); }
	[[nodiscard]] lug::parser const& parser() const { if (parser_stack_.empty()) throw parse_context_error{}; return *(parser_stack_.back()); }
	[[nodiscard]] unsigned int tab_width() const { return tab_width_; }
	void tab_width(unsigned int w) { tab_width_ = w; }
	[[nodiscard]] unsigned int tab_alignment() const { return tab_alignment_; }
	void tab_alignment(unsigned int a) { tab_alignment_ = a; }
	[[nodiscard]] bool has_condition(std::string_view name) const noexcept { return (conditions_.count(name) > 0); }
	bool set_condition(std::string_view name, bool value) { if (value) { return !conditions_.emplace(name).second; } else { return (conditions_.erase(name) > 0); } }
	void clear_conditions() { conditions_.clear(); }
	[[nodiscard]] bool has_symbol(std::string_view name) const noexcept { return (symbols_.count(name) > 0); }
	[[nodiscard]] std::vector<std::string> const& get_symbols(std::string_view name) const { auto it = symbols_.find(name); if (it == symbols_.end()) return empty_symbols_; return it->second; }
	void add_symbol(std::string_view name, std::string value) { symbols_[name].emplace_back(std::move(value)); }
	void clear_symbols(std::string_view name) { symbols_.erase(name); }
	[[nodiscard]] std::string_view match() const;
	[[nodiscard]] syntax_position position_at(std::size_t index);
	[[nodiscard]] syntax_position position_begin(syntax_range const& range);
	[[nodiscard]] syntax_position position_end(syntax_range const& range);
	[[nodiscard]] std::pair<syntax_position, syntax_position> position_range(syntax_range const& range);
	[[nodiscard]] unsigned int variable_instance() const { return (static_cast<unsigned int>(parse_depth()) << 16) | call_depth(); }
	[[nodiscard]] unsigned short parse_depth() const { return static_cast<unsigned short>(parser_stack_.size()); }
	[[nodiscard]] unsigned short call_depth() const;
	[[nodiscard]] unsigned short prune_depth() const;
	void escape();

	template <class T>
	void push_attribute(T&& x)
	{
		current_accept_context().emplace_back(std::in_place_type<T>, std::forward<T>(x));
	}

	template <class T, class... Args>
	void push_attribute(Args&&... args)
	{
		current_accept_context().emplace_back(std::in_place_type<T>, std::forward<Args>(args)...);
	}

	template <class T>
	[[nodiscard]] T pop_attribute()
	{
		return std::any_cast<T>(detail::pop_back(current_accept_context()));
	}
};

template <class T>
class variable
{
	environment& environment_;
	std::unordered_map<unsigned int, T> state_;
public:
	explicit variable(environment& e) : environment_{e} {}
	[[nodiscard]] T* operator->() { return &state_[environment_.variable_instance()]; }
	[[nodiscard]] T const* operator->() const { return &state_[environment_.variable_instance()]; }
	[[nodiscard]] T& operator*() { return state_[environment_.variable_instance()]; }
	[[nodiscard]] T const& operator*() const { return state_[environment_.variable_instance()]; }
};

class encoder
{
	directives mandate_;
	std::vector<directives> mode_;
	virtual void do_append(instruction) = 0;
	virtual void do_append(program const&) = 0;
	virtual immediate do_add_rune_set(unicode::rune_set) { return immediate{0}; }
	virtual immediate do_add_semantic_action(semantic_action) { return immediate{0}; }
	virtual immediate do_add_syntactic_capture(syntactic_capture) { return immediate{0}; }
	virtual immediate do_add_syntactic_predicate(syntactic_predicate) { return immediate{0}; }
	virtual void do_add_callee(rule const*, program const*, std::ptrdiff_t, directives) {}
	virtual bool do_should_evaluate_length() const noexcept { return true; }
	virtual std::ptrdiff_t do_length() const noexcept = 0;

protected:
	encoder& do_call(rule const* r, program const* p, std::ptrdiff_t off, unsigned short prec)
	{
		auto callee_mode = mode_.back();
		skip(p->mandate ^ directives::eps, directives::noskip);
		do_add_callee(r, p, length(), callee_mode);
		return encode(opcode::call, off, immediate{prec});
	}

	encoder& do_match(opcode op, std::string_view sequence)
	{
		while (sequence.size() > instruction::maxstrlen) {
			std::string_view subsequence = sequence.substr(0, instruction::maxstrlen);
			while (!subsequence.empty() && !utf8::is_lead(subsequence.back()))
				subsequence.remove_suffix(1);
			subsequence.remove_suffix(!subsequence.empty());
			encode(op, subsequence);
			sequence.remove_prefix(subsequence.size());
		}
		return encode(op, sequence);
	}

	template <class T>
	encoder& do_match_class(opcode op, T value)
	{
		constexpr auto penum = immediate{static_cast<unsigned short>(unicode::to_property_enum_v<std::decay_t<T>>)};
		return encode(op, detail::string_pack(value), penum);
	}

	void do_skip()
	{
		mode_.back() = (mode_.back() & ~(directives::preskip | directives::postskip)) | directives::lexeme | directives::noskip;
		grammar::implicit_space(*this);
	}

public:
	explicit encoder(directives initial) : mandate_{directives::none}, mode_{initial} {}
	virtual ~encoder() = default;
	template <class E> auto evaluate(E const& e) -> std::enable_if_t<is_expression_v<E>, encoder&>;
	template <class E> auto evaluate_length(E const& e) -> std::enable_if_t<is_expression_v<E>, std::ptrdiff_t>;
	encoder& dpsh(directives enable, directives disable) { directives prev = mode_.back(); mode_.push_back((prev & ~disable) | enable); return *this; }
	encoder& append(instruction instr) { do_append(instr); return *this; }
	encoder& append(program const& p) { do_append(p); return *this; }
	encoder& call(program const& p, unsigned short prec) { return do_call(nullptr, &p, 0, prec); }
	encoder& call(grammar const& g, unsigned short prec) { return do_call(nullptr, &g.program(), 3, prec); }
	encoder& encode(opcode op, immediate imm = immediate{0}) { return append(instruction{op, operands::none, imm}); }
	encoder& encode(opcode op, semantic_action a) { return append(instruction{op, operands::none, do_add_semantic_action(std::move(a))}); }
	encoder& encode(opcode op, syntactic_capture c) { return append(instruction{op, operands::none, do_add_syntactic_capture(std::move(c))}); }
	encoder& encode(opcode op, syntactic_predicate p) { return append(instruction{op, operands::none, do_add_syntactic_predicate(std::move(p))}); }
	encoder& encode(opcode op, std::ptrdiff_t off, immediate imm = immediate{0}) { return append(instruction{op, operands::off, imm}).append(instruction{off}); }
	[[nodiscard]] std::ptrdiff_t length() const noexcept { return do_length(); }
	[[nodiscard]] directives mandate() const noexcept { return (mandate_ & ~directives::eps) | mode_.back(); }
	[[nodiscard]] directives mode() const noexcept { return mode_.back(); }
	encoder& match(unicode::rune_set runes) { return skip().encode(opcode::match_set, do_add_rune_set(std::move(runes))); }
	encoder& match_eps() { return skip(directives::lexeme).encode(opcode::match); }
	encoder& match_any() { return skip().encode(opcode::match_any); }
	template <class T, class = std::enable_if_t<unicode::is_property_enum_v<T>>>
	encoder& match_any(T properties) { return skip().do_match_class(opcode::match_any_of, properties); }
	template <class T, class = std::enable_if_t<unicode::is_property_enum_v<T>>>
	encoder& match_all(T properties) { return skip().do_match_class(opcode::match_all_of, properties); }
	template <class T, class = std::enable_if_t<unicode::is_property_enum_v<T>>>
	encoder& match_none(T properties) { return skip().do_match_class(opcode::match_none_of, properties); }

	encoder& dpop(directives relay)
	{
		auto prev = detail::pop_back(mode_), next = (mode_.back() & ~relay) | (prev & relay);
		if ((next & directives::postskip) == directives::none && (prev & (directives::lexeme | directives::noskip | directives::postskip)) == directives::postskip)
			do_skip();
		mode_.back() = next;
		return *this;
	}

	encoder& skip(directives callee_mandate = directives::eps, directives callee_skip = directives::lexeme)
	{
		auto mode = mode_.back();
		if (mandate_ == directives::none)
			mandate_ = (mode & (directives::caseless | directives::lexeme | directives::noskip)) | directives::eps;
		if ((((mode | callee_mandate)) & (callee_skip | directives::preskip)) == directives::preskip)
			do_skip();
		mode_.back() = mode & ~(callee_mandate & directives::eps);
		return *this;
	}

	encoder& call(rule const& r, unsigned short prec, bool allow_inlining = true)
	{
		if (auto const& p = r.program_; allow_inlining && prec <= 0 && !r.currently_encoding_ && r.callees_.empty() && !p.instructions.empty() &&
					p.instructions.size() <= 8 && p.actions.size() <= 1 && p.captures.size() <= 1 && p.predicates.size() <= 1)
			return skip(p.mandate, directives::noskip).append(p);
		return do_call(&r, &r.program_, 0, prec);
	}

	encoder& encode(opcode op, std::string_view subsequence, immediate imm = immediate{0})
	{
		if (!subsequence.empty()) {
			detail::assure_in_range<resource_limit_error>(static_cast<unsigned short>(imm), 0u, instruction::maxstrlen - 1);
			detail::assure_in_range<resource_limit_error>(subsequence.size(), 1u, instruction::maxstrlen);
			do_append(instruction{op, operands::str, static_cast<immediate>((static_cast<unsigned short>(imm) << 8) | static_cast<unsigned short>(subsequence.size() - 1))});
			do {
				do_append(instruction{subsequence});
				subsequence.remove_prefix((std::min)(std::size_t{4}, subsequence.size()));
			} while (!subsequence.empty());
		}
		return *this;
	}

	encoder& match(std::string_view subject)
	{
		skip(!subject.empty() ? directives::eps : directives::none);
		if ((mode() & directives::caseless) != directives::none)
			return do_match(opcode::match_casefold, utf8::tocasefold(subject));
		else
			return do_match(opcode::match, subject);
	}
};

class instruction_length_evaluator final : public encoder
{
	std::ptrdiff_t length_;
	void do_append(instruction) final { length_ = detail::checked_add<program_limit_error>(length_, std::ptrdiff_t{1}); }
	void do_append(program const& p) final { length_ = detail::checked_add<program_limit_error>(length_, static_cast<std::ptrdiff_t>(p.instructions.size())); }
	bool do_should_evaluate_length() const noexcept final { return false; }
	std::ptrdiff_t do_length() const noexcept final { return length_; }
public:
	explicit instruction_length_evaluator(directives initial) : encoder{initial}, length_{0} {}
	~instruction_length_evaluator() final = default;
};

class program_encoder : public encoder
{
	program& program_;
	program_callees& callees_;
	std::ptrdiff_t do_length() const noexcept final { return static_cast<std::ptrdiff_t>(program_.instructions.size()); }
	void do_append(instruction instr) final { program_.instructions.push_back(instr); }
	void do_append(program const& p) final { program_.concatenate(p); }
	void do_add_callee(rule const* r, program const* p, std::ptrdiff_t n, directives d) final { callees_.emplace_back(r, p, n, d); }
	immediate do_add_rune_set(unicode::rune_set r) final { return add_item(program_.runesets, std::move(r)); }
	immediate do_add_semantic_action(semantic_action a) final { return add_item(program_.actions, std::move(a)); }
	immediate do_add_syntactic_capture(syntactic_capture a) final { return add_item(program_.captures, std::move(a)); }
	immediate do_add_syntactic_predicate(syntactic_predicate p) final { return add_item(program_.predicates, std::move(p)); }

	template <class Item>
	immediate add_item(std::vector<Item>& items, Item&& item)
	{
		detail::assure_in_range<resource_limit_error>(items.size(), 0u, (std::numeric_limits<unsigned short>::max)() - 1u);
		items.push_back(std::forward<Item>(item));
		return static_cast<immediate>(items.size() - 1);
	}

public:
	program_encoder(program& p, program_callees& c, directives initial) : encoder{initial}, program_{p}, callees_{c} {}
	~program_encoder() override { program_.mandate = mandate(); }
};

class rule_encoder final : public program_encoder
{
	rule& rule_;
public:
	explicit rule_encoder(rule& r) : program_encoder{r.program_, r.callees_, directives::eps}, rule_{r} { rule_.currently_encoding_ = true; }
	~rule_encoder() final { rule_.currently_encoding_ = false; }
};

template <class RuneSet>
inline auto&& add_rune_range(RuneSet&& runes, directives mode, char32_t first, char32_t last)
{
	if (first > last)
		throw bad_character_range{};
	if ((mode & directives::caseless) != directives::none)
		unicode::push_casefolded_range(runes, first, last);
	else
		unicode::push_range(runes, first, last);
	return std::move(runes);
}

class basic_regular_expression
{
	std::string const expression_;
	std::shared_ptr<program> const program_;

	[[nodiscard]] static grammar make_grammar();

	struct generator : environment
	{
		basic_regular_expression const& owner;
		program_callees callees;
		program_encoder encoder;
		bool circumflex = false;
		unicode::ctype classes = unicode::ctype::none;
		unicode::rune_set runes;

		generator(basic_regular_expression const& se, directives mode)
			: owner{se}, encoder{*se.program_, callees, mode | directives::eps | directives::lexeme}
		{}

		void bracket_class(std::string_view s)
		{
			if (auto c = unicode::stoctype(s); c.has_value())
				classes |= c.value();
			else
				throw bad_character_class{};
		}

		void bracket_range(std::string_view s)
		{
			bracket_range(s.substr(0, s.find('-')), s.substr(s.find('-') + 1));
		}

		void bracket_range(std::string_view a, std::string_view b)
		{
			add_rune_range(std::ref(runes), encoder.mode(),
				utf8::decode_rune(std::begin(a), std::end(a)).second,
				utf8::decode_rune(std::begin(b), std::end(b)).second);
		}

		void bracket_commit()
		{
			runes = unicode::sort_and_optimize(std::move(runes));
			if (!runes.empty() && classes == unicode::ctype::none) {
				if (circumflex)
					runes = unicode::negate(runes);
				encoder.match(std::move(runes));
			} else {
				if (circumflex)
					encoder.encode(opcode::choice, 3 + (!runes.empty() ? 1 : 0) + (classes != unicode::ctype::none ? 1 : 0));
				if (!runes.empty())
					encoder.match(std::move(runes));
				if (classes != unicode::ctype::none)
					encoder.match_any(classes);
				if (circumflex)
					encoder.encode(opcode::commit, 0).encode(opcode::fail).match_any();
			}
			runes.clear(), classes = unicode::ctype::none, circumflex = false;
		}
	};

public:
	explicit basic_regular_expression(std::string_view e) : expression_{e}, program_{std::make_shared<program>()} {}
	void operator()(encoder& d) const;
};

class string_expression
{
	std::string_view const expression_;
public:
	constexpr explicit string_expression(std::string_view e) noexcept : expression_{e} {}
	void operator()(encoder& d) const { d.match(expression_); }
};

template <class T, class E = std::remove_cv_t<std::remove_reference_t<T>>, class = std::enable_if_t<is_proper_expression_v<E>>>
[[nodiscard]] constexpr T const& make_expression(T const& t)
{
	return t;
}

template <class T, class E = std::remove_cv_t<std::remove_reference_t<T>>, class = std::enable_if_t<!is_proper_expression_v<E>>>
[[nodiscard]] constexpr auto make_expression(T&& t)
{
	static_assert(is_expression_v<E>, "T must be an expression type");
	if constexpr (is_callable_v<E>)
		return [&target = t](encoder& d) { d.call(target, 0); };
	else if constexpr (is_predicate_v<E>)
		return [p = syntactic_predicate{std::forward<T>(t)}](encoder& d) { d.encode(opcode::predicate, p); };
	else if constexpr (is_string_expression_v<E>)
		return string_expression{std::forward<T>(t)};
}

template <class E>
inline auto encoder::evaluate(E const& e) -> std::enable_if_t<is_expression_v<E>, encoder&>
{
	make_expression(e)(*this);
	return *this;
}

template <class E>
inline auto encoder::evaluate_length(E const& e) -> std::enable_if_t<is_expression_v<E>, std::ptrdiff_t>
{
	return do_should_evaluate_length() ? instruction_length_evaluator{mode()}.evaluate(e).length() : 0;
}

template <class E, class>
inline rule::rule(E const& e)
{
	rule_encoder{*this}.evaluate(e);
}

inline rule::rule(rule const& r)
{
	rule_encoder{*this}.call(r, 1);
}

[[nodiscard]] inline auto rule::operator[](unsigned short precedence) const noexcept
{
	return [this, precedence](encoder& d){ d.call(*this, precedence); };
}

template <directives EnableMask, directives DisableMask, directives RelayMask>
struct directive_modifier
{
	template <class E, class = std::enable_if_t<is_expression_v<E>>>
	[[nodiscard]] constexpr auto operator[](E const& e) const
	{
		return [e = make_expression(e)](encoder& d) { d.dpsh(EnableMask, DisableMask).evaluate(e).dpop(RelayMask); };
	}
};

inline constexpr directive_modifier<directives::none, directives::none, directives::none> matches_eps{};
inline constexpr directive_modifier<directives::none, directives::none, directives::eps> relays_eps{};
inline constexpr directive_modifier<directives::postskip, directives::none, directives::eps> skip_after{};
inline constexpr directive_modifier<directives::preskip, directives::postskip, directives::eps> skip_before{};
template <unicode::ctype Property> struct ctype_combinator { void operator()(encoder& d) const { d.match_any(Property); } };

template <bool Value>
struct condition_test_combinator
{
	[[nodiscard]] constexpr auto operator()(std::string_view name) const noexcept
	{
		return [name](encoder& d) {
			d.encode(opcode::condition_test, name, immediate{Value ? 1 : 0});
		};
	}
};

template <bool Value>
struct condition_block_combinator
{
	struct condition_block_expression
	{
		std::string_view name;

		template <class E, class = std::enable_if_t<is_expression_v<E>>>
		[[nodiscard]] constexpr auto operator[](E const& e) const
		{
			return [e = make_expression(e), n = name](encoder& d) {
				d.encode(opcode::condition_push, n, immediate{Value ? 1 : 0}).evaluate(e).encode(opcode::condition_pop);
			};
		}
	};

	[[nodiscard]] constexpr auto operator()(std::string_view name) const noexcept { return condition_block_expression{name}; }
};

template <bool Value>
struct symbol_exists_combinator
{
	[[nodiscard]] constexpr auto operator()(std::string_view name) const noexcept
	{
		return [name](encoder& d) {
			d.encode(opcode::symbol_exists, name, immediate{Value ? 1 : 0});
		};
	}
};

template <opcode Op>
struct symbol_match_combinator
{
	[[nodiscard]] constexpr auto operator()(std::string_view name) const noexcept
	{
		return [name](encoder& d) {
			d.skip(directives::eps).encode(Op, name);
		};
	}
};

template <opcode Op>
struct symbol_match_offset_combinator
{
	[[nodiscard]] constexpr auto operator()(std::string_view name, std::size_t offset = 0) const noexcept
	{
		if (offset > (std::numeric_limits<unsigned char>::max)())
			throw resource_limit_error{};
		return [name, offset](encoder& d) {
			d.skip(directives::eps).encode(Op, name, immediate{static_cast<unsigned short>(offset)});
		};
	}
};

namespace language {

using lug::grammar; using lug::rule; using lug::start;
using unicode::ctype; using unicode::ptype; using unicode::gctype; using unicode::sctype;
using unicode::blktype; using unicode::agetype; using unicode::eawtype;
using syntax = lug::syntax; using syntax_position = lug::syntax_position; using syntax_range = lug::syntax_range;
using parser = lug::parser; using environment = lug::environment; template <class T> using variable = lug::variable<T>;
inline constexpr directive_modifier<directives::none, directives::caseless, directives::eps> cased{};
inline constexpr directive_modifier<directives::caseless, directives::none, directives::eps> caseless{};
inline constexpr directive_modifier<directives::lexeme, directives::noskip, directives::eps> lexeme{};
inline constexpr directive_modifier<directives::lexeme | directives::noskip, directives::none, directives::eps> noskip{};
inline constexpr directive_modifier<directives::none, directives::lexeme | directives::noskip, directives::eps> skip{};
inline constexpr struct { void operator()(encoder&) const {} } nop{};
inline constexpr struct { void operator()(encoder& d) const { d.match_eps(); } } eps{};
inline constexpr struct { void operator()(encoder& d) const { d.encode(opcode::choice, 2).encode(opcode::match_any).encode(opcode::fail, immediate{1}); } } eoi{};
inline constexpr struct { void operator()(encoder& d) const { d.encode(opcode::match_eol); } } eol{};
inline constexpr struct { void operator()(encoder& d) const { d.encode(opcode::accept); } } cut{};
inline constexpr ctype_combinator<ctype::alpha> alpha{}; inline constexpr ctype_combinator<ctype::alnum> alnum{}; inline constexpr ctype_combinator<ctype::lower> lower{};
inline constexpr ctype_combinator<ctype::upper> upper{}; inline constexpr ctype_combinator<ctype::digit> digit{}; inline constexpr ctype_combinator<ctype::xdigit> xdigit{};
inline constexpr ctype_combinator<ctype::space> space{}; inline constexpr ctype_combinator<ctype::blank> blank{}; inline constexpr ctype_combinator<ctype::punct> punct{};
inline constexpr ctype_combinator<ctype::graph> graph{}; inline constexpr ctype_combinator<ctype::print> print{};
inline constexpr condition_test_combinator<true> when{}; inline constexpr condition_test_combinator<false> unless{};
inline constexpr condition_block_combinator<true> on{}; inline constexpr condition_block_combinator<false> off{};
inline constexpr symbol_exists_combinator<true> exists{}; inline constexpr symbol_exists_combinator<false> missing{};
inline constexpr symbol_match_offset_combinator<opcode::symbol_head> match_front{}; inline constexpr symbol_match_offset_combinator<opcode::symbol_tail> match_back{};
inline constexpr symbol_match_combinator<opcode::symbol_all> match_all{}; inline constexpr symbol_match_combinator<opcode::symbol_any> match_any{};
inline constexpr symbol_match_combinator<opcode::symbol_tail> match{};

inline constexpr struct
{
	void operator()(encoder& d) const { d.match_any(); }
	template <class T, class = std::enable_if_t<unicode::is_property_enum_v<T>>>
	[[nodiscard]] constexpr auto operator()(T p) const { return [p](encoder& d) { d.match_any(p); }; }
}
any{};

inline constexpr struct
{
	template <class T, class = std::enable_if_t<unicode::is_property_enum_v<T>>>
	[[nodiscard]] constexpr auto operator()(T p) const { return [p](encoder& d) { d.match_all(p); }; }
}
all{};

inline constexpr struct
{
	template <class T, class = std::enable_if_t<unicode::is_property_enum_v<T>>>
	[[nodiscard]] constexpr auto operator()(T p) const { return [p](encoder& d) { d.match_none(p); }; }
}
none{};

inline constexpr struct
{
	[[nodiscard]] auto operator()(std::string_view s) const { return basic_regular_expression{s}; }
	[[nodiscard]] auto operator()(char const* s, std::size_t n) const { return basic_regular_expression{std::string_view{s, n}}; }
}
bre{};

inline constexpr struct
{
	[[nodiscard]] constexpr auto operator()(char c) const
	{
		return [c](encoder& d) { d.match(std::string_view{&c, 1}); };
	}

	[[nodiscard]] constexpr auto operator()(char32_t c) const
	{
		return [c](encoder& d) { d.match(utf8::encode_rune(c)); };
	}

	[[nodiscard]] constexpr auto operator()(char32_t start, char32_t end) const
	{
		return [start, end](encoder& d) {
			d.match(unicode::sort_and_optimize(add_rune_range(unicode::rune_set{}, d.mode(), start, end)));
		};
	}
}
chr{};

inline constexpr struct
{
	[[nodiscard]] constexpr auto operator()(std::string_view s) const noexcept { return string_expression{s}; }
	[[nodiscard]] constexpr auto operator()(char const* s, std::size_t n) const noexcept { return string_expression{std::string_view{s, n}}; }
}
str{};

[[nodiscard]] constexpr auto operator ""_cx(char c) { return chr(c); }
[[nodiscard]] constexpr auto operator ""_cx(char32_t c) { return chr(c); }
[[nodiscard]] constexpr auto operator ""_sx(char const* s, std::size_t n) { return string_expression{std::string_view{s, n}}; }
[[nodiscard]] inline auto operator ""_rx(char const* s, std::size_t n) { return basic_regular_expression{std::string_view{s, n}}; }
[[nodiscard]] constexpr auto operator ""_icx(char c) { return caseless[chr(c)]; }
[[nodiscard]] constexpr auto operator ""_icx(char32_t c) { return caseless[chr(c)]; }
[[nodiscard]] constexpr auto operator ""_isx(char const* s, std::size_t n) { return caseless[string_expression{std::string_view{s, n}}]; }
[[nodiscard]] inline auto operator ""_irx(char const* s, std::size_t n) { return caseless[basic_regular_expression{std::string_view{s, n}}]; }
[[nodiscard]] constexpr auto operator ""_scx(char c) { return cased[chr(c)]; }
[[nodiscard]] constexpr auto operator ""_scx(char32_t c) { return cased[chr(c)]; }
[[nodiscard]] constexpr auto operator ""_ssx(char const* s, std::size_t n) { return cased[string_expression{std::string_view{s, n}}]; }
[[nodiscard]] inline auto operator ""_srx(char const* s, std::size_t n) { return cased[basic_regular_expression{std::string_view{s, n}}]; }

struct implicit_space_rule
{
	std::function<void(encoder&)> prev_rule;
	template <class E, class = std::enable_if_t<is_expression_v<E>>>
	implicit_space_rule(E const& e) : prev_rule{std::exchange(grammar::implicit_space, std::function<void(encoder&)>{make_expression(e)})} {}
	~implicit_space_rule() { grammar::implicit_space = std::move(prev_rule); }
};

template <class E, class = std::enable_if_t<is_expression_v<E>>>
[[nodiscard]] constexpr auto operator!(E const& e)
{
	return [x = matches_eps[e]](encoder& d) {
		d.encode(opcode::choice, 1 + d.evaluate_length(x)).evaluate(x).encode(opcode::fail, immediate{1});
	};
}

template <class E, class = std::enable_if_t<is_expression_v<E>>>
[[nodiscard]] constexpr auto operator&(E const& e)
{
	return [x = matches_eps[e]](encoder& d) {
		d.encode(opcode::choice, 2 + d.evaluate_length(x)).evaluate(x).encode(opcode::commit_back, 1).encode(opcode::fail);
	};
}

template <class E, class = std::enable_if_t<is_expression_v<E>>>
[[nodiscard]] constexpr auto operator*(E const& e)
{
	return [x = matches_eps[skip_after[e]]](encoder& d) {
		auto n = d.evaluate_length(x);
		d.encode(opcode::choice, 2 + n).evaluate(x).encode(opcode::commit_partial, -(2 + n));
	};
}

template <class E1, class E2, class = std::enable_if_t<is_expression_v<E1> && is_expression_v<E2>>>
[[nodiscard]] constexpr auto operator|(E1 const& e1, E2 const& e2)
{
	return [x1 = relays_eps[e1], x2 = relays_eps[e2]](encoder& d) {
		d.encode(opcode::choice, 2 + d.evaluate_length(x1)).evaluate(x1).encode(opcode::commit, d.evaluate_length(x2)).evaluate(x2);
	};
}

template <class E1, class E2, class = std::enable_if_t<is_expression_v<E1> && is_expression_v<E2>>>
[[nodiscard]] constexpr auto operator>(E1 const& e1, E2 const& e2)
{
	return [x1 = make_expression(e1), x2 = skip_before[e2]](encoder& d) { d.evaluate(x1).evaluate(x2); };
}

template <class E, class A, class = std::enable_if_t<is_expression_v<E>>>
[[nodiscard]] constexpr auto operator<(E const& e, A a)
{
	if constexpr (std::is_invocable_v<A, environment&, syntax>) {
		return [e = make_expression(e), a = std::move(a)](encoder& d) {
			d.skip().encode(opcode::capture_start).evaluate(e).encode(opcode::capture_end, syntactic_capture{a});
		};
	} else if constexpr (std::is_invocable_v<A, detail::dynamic_cast_if_base_of<environment&>, syntax>) {
		return e < [a = std::move(a)](environment& envr, syntax const& x) {
			a(detail::dynamic_cast_if_base_of<environment&>{envr}, x);
		};
	} else if constexpr (std::is_invocable_v<A, syntax>) {
		return e < [a = std::move(a)](environment&, syntax const& x) {
			a(x);
		};
	} else if constexpr (std::is_invocable_v<A, environment&>) {
		return [e = make_expression(e), a = std::move(a)](encoder& d) {
			d.evaluate(e).encode(opcode::action, semantic_action{a});
		};
	} else if constexpr (std::is_invocable_v<A, detail::dynamic_cast_if_base_of<environment&>>) {
		return e < [a = std::move(a)](environment& envr) {
			a(detail::dynamic_cast_if_base_of<environment&>{envr});
		};
	} else if constexpr (std::is_invocable_v<A> && std::is_same_v<void, std::invoke_result_t<A>>) {
		return [e = make_expression(e), a = std::move(a)](encoder& d) {
			d.evaluate(e).encode(opcode::action, [a](environment&) { a(); });
		};
	} else if constexpr (std::is_invocable_v<A>) {
		return [e = make_expression(e), a = std::move(a)](encoder& d) {
			d.evaluate(e).encode(opcode::action, [a](environment& envr) { envr.push_attribute(a()); });
		};
	}
}

template <class E, class = std::enable_if_t<is_expression_v<E>>>
[[nodiscard]] constexpr auto operator+(E const& e)
{
	auto x = make_expression(e);
	return x > *x;
}

template <class E, class = std::enable_if_t<is_expression_v<E>>>
[[nodiscard]] constexpr auto operator~(E const& e)
{
	return e | eps;
}

template <class E, class = std::enable_if_t<is_expression_v<E>>>
[[nodiscard]] constexpr auto operator--(E const& e)
{
	return cut > e;
}

template <class E, class = std::enable_if_t<is_expression_v<E>>>
[[nodiscard]] constexpr auto operator--(E const& e, int)
{
	return e > cut;
}

inline constexpr struct
{
	template <class T>
	struct capture_to
	{
		variable<T>& v;

		template <class E, class = std::enable_if_t<is_expression_v<E>>>
		[[nodiscard]] constexpr auto operator[](E const& e) const
		{
			return e < [&vr = v](syntax x) { *vr = T(x.capture()); };
		}
	};

	template <class T>
	[[nodiscard]] constexpr auto operator()(variable<T>& v) const
	{
		return capture_to<T>{v};
	}
}
capture{};

template <class T, class E, class = std::enable_if_t<is_expression_v<E>>>
[[nodiscard]] constexpr auto operator%(variable<T>& v, E const& e)
{
	return e < [&v](environment& s) { *v = s.pop_attribute<T>(); };
}

inline constexpr struct
{
	struct capture_to
	{
		std::string_view name;

		template <class E, class = std::enable_if_t<is_expression_v<E>>>
		[[nodiscard]] constexpr auto operator[](E const& e) const
		{
			return [e = make_expression(e), n = name](encoder& d) {
				d.skip().encode(opcode::symbol_start, n).evaluate(e).encode(opcode::symbol_end);
			};
		}
	};

	[[nodiscard]] constexpr auto operator()(std::string_view name) const noexcept { return capture_to{name}; }
}
symbol{};

inline constexpr struct
{
	template <class E, class = std::enable_if_t<is_expression_v<E>>>
	[[nodiscard]] constexpr auto operator[](E const& e) const
	{
		return [e = make_expression(e)](encoder& d) {
			d.skip().encode(opcode::symbol_push).evaluate(e).encode(opcode::symbol_pop);
		};
	}
}
block{};

inline constexpr struct
{
	struct local_to
	{
		std::string_view name;

		template <class E, class = std::enable_if_t<is_expression_v<E>>>
		[[nodiscard]] constexpr auto operator[](E const& e) const
		{
			return [e = make_expression(e), n = name](encoder& d) {
				d.skip().encode(opcode::symbol_push, n, immediate{1}).evaluate(e).encode(opcode::symbol_pop);
			};
		}
	};

	[[nodiscard]] constexpr auto operator()(std::string_view name) const noexcept { return local_to{name}; }

	template <class E, class = std::enable_if_t<is_expression_v<E>>>
	[[nodiscard]] constexpr auto operator[](E const& e) const
	{
		return [e = make_expression(e)](encoder& d) {
			d.skip().encode(opcode::symbol_push, immediate{2}).evaluate(e).encode(opcode::symbol_pop);
		};
	}
}
local{};

} // namespace language

inline thread_local std::function<void(encoder&)> grammar::implicit_space{language::operator*(language::space)};

[[nodiscard]] inline grammar start(rule const& start_rule)
{
	program grprogram;
	program_callees grcallees;
	std::unordered_map<program const*, std::ptrdiff_t> addresses;
	std::vector<std::pair<program const*, std::ptrdiff_t>> calls;
	std::unordered_set<program const*> left_recursive;
	std::vector<std::pair<std::vector<std::pair<rule const*, bool>>, program const*>> unprocessed;
	program_encoder{grprogram, grcallees, directives::eps | directives::preskip}.call(start_rule, 1, false).encode(opcode::accept_final);
	calls.emplace_back(&start_rule.program_, std::get<2>(grcallees.back()));
	unprocessed.emplace_back(std::vector<std::pair<rule const*, bool>>{{&start_rule, false}}, &start_rule.program_);
	do {
		auto [callstack, subprogram] = detail::pop_back(unprocessed);
		auto const address = static_cast<std::ptrdiff_t>(grprogram.instructions.size());
		if (addresses.emplace(subprogram, address).second) {
			grprogram.concatenate(*subprogram);
			grprogram.instructions.emplace_back(opcode::ret, operands::none, immediate{0});
			if (auto top_rule = callstack.back().first; top_rule) {
				for (auto [callee_rule, callee_program, instr_offset, mode] : top_rule->callees_) {
					calls.emplace_back(callee_program, address + instr_offset);
					if (callee_rule && (mode & directives::eps) != directives::none && detail::escaping_find_if(
							callstack.crbegin(), callstack.crend(), [rule = callee_rule](auto& caller) {
								return caller.first == rule ? 1 : (caller.second ? 0 : -1); }) != callstack.crend()) {
						left_recursive.insert(callee_program);
					} else {
						auto callee_callstack = callstack;
						callee_callstack.emplace_back(callee_rule, (mode & directives::eps) != directives::none);
						unprocessed.emplace_back(std::move(callee_callstack), callee_program);
					}
				}
			}
		}
	} while (!unprocessed.empty());
	for (auto [subprogram, instr_addr] : calls) {
		if (auto& iprefix = grprogram.instructions[static_cast<std::size_t>(instr_addr)]; iprefix.pf.op == opcode::call)
			iprefix.pf.val = left_recursive.count(subprogram) != 0 ? (iprefix.pf.val != 0 ? iprefix.pf.val : 1) : 0;
		auto& ioffset = grprogram.instructions[static_cast<std::size_t>(instr_addr + 1)];
		auto const rel_addr = ioffset.off + addresses[subprogram] - (instr_addr + 2);
		detail::assure_in_range<program_limit_error>(rel_addr, std::numeric_limits<int>::lowest(), (std::numeric_limits<int>::max)());
		ioffset.off = static_cast<int>(rel_addr);
	}
	return grammar{std::move(grprogram)};
}

struct parser_registers
{
	std::size_t sr, mr, rc; std::ptrdiff_t pc; std::size_t fc;
	[[nodiscard]] auto as_tuple() noexcept { return std::forward_as_tuple(sr, mr, rc, pc, fc); }
	[[nodiscard]] auto as_tuple() const noexcept { return std::forward_as_tuple(sr, mr, rc, pc, fc); }
};

class parser
{
	enum class stack_frame_type : unsigned char { backtrack, call, capture, condition, lrcall, symbol_definition, symbol_table };
	enum class subject_location : std::size_t {};
	struct lrmemo { std::size_t srr, sra, prec; std::ptrdiff_t pcr, pca; std::size_t rcr; std::vector<semantic_response> responses; };
	static inline constexpr std::size_t lrfailcode = (std::numeric_limits<std::size_t>::max)();
	static inline constexpr unsigned short max_call_depth = (std::numeric_limits<unsigned short>::max)();
	static inline constexpr std::size_t max_size = (std::numeric_limits<std::size_t>::max)();
	lug::grammar const& grammar_;
	lug::environment& environment_;
	std::vector<std::function<bool(std::string&)>> sources_;
	std::string input_;
	std::unordered_map<std::size_t, std::string> casefolded_subjects_;
	syntax_position origin_{1, 1};
	std::vector<std::pair<std::size_t, syntax_position>> positions_;
	parser_registers registers_{0, 0, 0, 0, 0};
	bool parsing_{false}, reading_{false}, cut_deferred_{false};
	std::size_t cut_frame_{0};
	std::vector<stack_frame_type> stack_frames_;
	std::vector<std::tuple<std::size_t, std::size_t, std::ptrdiff_t>> backtrack_stack_; // sr, rc, pc
	std::vector<std::ptrdiff_t> call_stack_; // pc
	std::vector<subject_location> capture_stack_; // sr
	std::vector<std::pair<std::string_view, bool>> condition_stack_; // name, value
	std::vector<std::pair<std::string_view, subject_location>> symbol_definition_stack_; // name, sr
	std::vector<std::unordered_map<std::string_view, std::vector<std::string>>> symbol_table_stack_;
	std::vector<lrmemo> lrmemo_stack_;
	std::vector<semantic_response> responses_;
	unsigned short prune_depth_{max_call_depth}, call_depth_{0};

	[[nodiscard]] bool available(std::size_t sr, std::size_t sn)
	{
		do {
			if (sn <= (input_.size() - sr))
				return true;
			if (sr < input_.size())
				return false;
		} while (read_more());
		return false;
	}

	[[nodiscard]] bool read_more()
	{
		if (sources_.empty())
			return false;
		std::string text;
		detail::reentrancy_sentinel<reenterant_read_error> const guard{reading_};
		while (!sources_.empty() && text.empty()) {
			text.clear();
			bool const more = sources_.back()(text);
			input_.insert(input_.end(), text.begin(), text.end());
			if (!more)
				sources_.pop_back();
		}
		return !text.empty();
	}

	[[nodiscard]] int casefold_compare(std::size_t sr, std::size_t sn, std::string_view str)
	{
		auto& subject = casefolded_subjects_[sr];
		if (subject.size() < sn)
			subject = utf8::tocasefold(std::string_view{input_.data() + sr, sn});
		return subject.compare(0, sn, str);
	}

	template <class Compare>
	[[nodiscard]] bool match_sequence(std::size_t& sr, std::string_view str, Compare&& comp)
	{
		if (auto sn = str.size(); !sn || (available(sr, sn) && comp(sr, sn, str))) {
			sr += sn;
			return true;
		}
		return false;
	}

	template <class Match>
	[[nodiscard]] bool match_single(std::size_t& sr, Match&& match)
	{
		if (!available(sr, 1))
			return false;
		auto const curr = input_.cbegin() + static_cast<std::ptrdiff_t>(sr), last = input_.cend();
		auto [next, rune] = utf8::decode_rune(curr, last);
		bool matched;
		if constexpr (std::is_invocable_v<Match, decltype(curr), decltype(last), decltype(next)&, char32_t>) {
			matched = match(curr, last, next, rune);
		} else if constexpr(std::is_invocable_v<Match, unicode::record const&>) {
			matched = match(unicode::query(rune));
		} else if constexpr(std::is_invocable_v<Match, char32_t>) {
			matched = match(rune);
		} else {
			matched = match();
			detail::ignore(rune);
		}
		if (matched)
			sr += static_cast<std::size_t>(std::distance(curr, next));
		return matched;
	}

	template <opcode Opcode>
	[[nodiscard]] bool commit(std::size_t& sr, std::size_t& rc, std::ptrdiff_t& pc, int off)
	{
		if (stack_frames_.empty() || stack_frames_.back() != stack_frame_type::backtrack)
			return false;
		if constexpr (Opcode == opcode::commit_partial) {
			detail::make_tuple_view<0, 1>(backtrack_stack_.back()) = {sr, rc};
		} else {
			detail::ignore(sr, rc);
			if constexpr (Opcode == opcode::commit_back)
				sr = std::get<0>(backtrack_stack_.back());
			pop_stack_frame(backtrack_stack_);
		}
		pc += off;
		return true;
	}

	void accept(std::size_t sr, std::size_t mr, std::size_t rc, std::ptrdiff_t pc)
	{
		registers_ = {sr, (std::max)(mr, sr), rc, pc, 0};
		environment_.start_accept();
		detail::scope_exit const cleanup{[this]{ environment_.end_accept(); }};
		auto const& actions = grammar_.program().actions;
		auto const& captures = grammar_.program().captures;
		auto const full_match = match();
		for (auto& response : responses_) {
			if (prune_depth_ <= response.call_depth)
				continue;
			prune_depth_ = max_call_depth, call_depth_ = response.call_depth;
			if (response.range.index < max_size)
				captures[response.action_index](environment_, syntax{full_match.substr(response.range.index, response.range.size), response.range.index});
			else
				actions[response.action_index](environment_);
		}
	}

	[[nodiscard]] auto drain()
	{
		origin_ = position_at(registers_.sr);
		input_.erase(0, registers_.sr);
		casefolded_subjects_.clear();
		positions_.clear();
		responses_.clear();
		registers_.mr -= registers_.sr;
		registers_.sr = 0, registers_.rc = 0;
		cut_deferred_ = false, cut_frame_ = stack_frames_.size();
		return registers_.as_tuple();
	}

	void pop_responses_after(std::size_t n)
	{
		if (n < responses_.size())
			responses_.resize(n);
	}

	[[nodiscard]] auto drop_responses_after(std::size_t n)
	{
		std::vector<semantic_response> dropped;
		if (n < responses_.size()) {
			dropped.assign(responses_.begin() + static_cast<std::ptrdiff_t>(n), responses_.end());
			responses_.resize(n);
		}
		return dropped;
	}

	[[nodiscard]] auto restore_responses_after(std::size_t n, std::vector<semantic_response> const& restore)
	{
		pop_responses_after(n);
		responses_.insert(responses_.end(), restore.begin(), restore.end());
		return responses_.size();
	}

	[[nodiscard]] auto push_response(std::size_t depth, std::size_t action_index, syntax_range range = {max_size, 0})
	{
		responses_.push_back({static_cast<unsigned short>(depth), static_cast<unsigned short>(action_index), range});
		return responses_.size();
	}

	template <class Stack, class... Args>
	void pop_stack_frame(Stack& stack, Args&... args)
	{
		stack.pop_back(), stack_frames_.pop_back();
		cut_frame_ = (std::min)(cut_frame_, stack_frames_.size());
		if constexpr (std::is_same_v<typename Stack::value_type, subject_location> || std::is_same_v<typename Stack::value_type, lrmemo>)
			if (cut_deferred_ && capture_stack_.empty() && lrmemo_stack_.empty())
				accept(args...);
	}

public:
	parser(lug::grammar const& g, lug::environment& e) : grammar_{g}, environment_{e} {}
	[[nodiscard]] lug::grammar const& grammar() const noexcept { return grammar_; }
	[[nodiscard]] lug::environment& environment() const noexcept { return environment_; }
	[[nodiscard]] std::string_view match() const noexcept { return {input_.data(), registers_.sr}; }
	[[nodiscard]] std::string_view subject() const noexcept { return {input_.data() + registers_.sr, input_.size() - registers_.sr}; }
	[[nodiscard]] std::size_t subject_index() const noexcept { return registers_.sr; }
	[[nodiscard]] std::size_t max_subject_index() const noexcept { return registers_.mr; }
	[[nodiscard]] syntax_position subject_position() { return position_at(registers_.sr); }
	[[nodiscard]] syntax_position max_subject_position() { return position_at(registers_.mr); }
	[[nodiscard]] syntax_position position_begin(syntax_range const& range) { return position_at(range.index); }
	[[nodiscard]] syntax_position position_end(syntax_range const& range) { return position_at(range.index + range.size); }
	[[nodiscard]] std::pair<syntax_position, syntax_position> position_range(syntax_range const& range) { return {position_begin(range), position_end(range)}; }
	[[nodiscard]] parser_registers& registers() noexcept { return registers_; }
	[[nodiscard]] parser_registers const& registers() const noexcept { return registers_; }
	[[nodiscard]] bool available(std::size_t sn) { return available(registers_.sr, sn); }
	[[nodiscard]] unsigned short call_depth() const noexcept { return call_depth_; }
	[[nodiscard]] unsigned short prune_depth() const noexcept { return prune_depth_; }
	void escape() { prune_depth_ = call_depth_; }

	[[nodiscard]] syntax_position position_at(std::size_t index)
	{
		auto const pos = std::lower_bound(std::begin(positions_), std::end(positions_), index, [](auto& x, auto& y) { return x.first < y; });
		if (pos != std::end(positions_) && index == pos->first)
			return pos->second;
		std::size_t startindex = 0;
		syntax_position position = origin_;
		if (pos != std::begin(positions_)) {
			auto prevpos = std::prev(pos);
			startindex = prevpos->first;
			position = prevpos->second;
		}
		auto first = std::next(std::begin(input_), static_cast<std::ptrdiff_t>(startindex));
		auto const last = std::next(std::begin(input_), static_cast<std::ptrdiff_t>(index));
		char32_t rune, prevrune = U'\0';
		for (auto curr = first, next = curr; curr < last; curr = next, prevrune = rune) {
			std::tie(next, rune) = utf8::decode_rune(curr, last);
			if ((unicode::query(rune).properties() & unicode::ptype::Line_Ending) != unicode::ptype::None && (prevrune != U'\r' || rune != U'\n')) {
				position.column = 1, ++position.line;
				first = next;
			}
		}
		for (auto curr = first, next = curr; curr < last; curr = next) {
			std::tie(next, rune) = utf8::decode_rune(curr, last);
			if (rune != U'\t') {
				position.column += unicode::ucwidth(rune);
			} else {
				auto const oldcolumn = position.column;
				auto const newcolumn = oldcolumn + environment_.tab_width();
				auto const alignedcolumn = newcolumn - ((newcolumn - 1) % environment_.tab_alignment());
				position.column = (std::max)((std::min)(newcolumn, alignedcolumn), oldcolumn);
			}
		}
		return positions_.insert(pos, std::make_pair(index, position))->second;
	}

	template <class InputIt, class = utf8::enable_if_char_input_iterator_t<InputIt>>
	parser& enqueue(InputIt first, InputIt last)
	{
		input_.insert(input_.end(), first, last);
		return *this;
	}

	template <class InputFunc, class = std::enable_if_t<std::is_invocable_r_v<bool, InputFunc, std::string&>>>
	parser& push_source(InputFunc&& func)
	{
		if (reading_)
			throw reenterant_read_error{};
		sources_.emplace_back(std::forward<InputFunc>(func));
		return *this;
	}

	template <class InputIt, class = utf8::enable_if_char_input_iterator_t<InputIt>>
	bool parse(InputIt first, InputIt last)
	{
		return enqueue(first, last).parse();
	}

	template <class InputFunc, class = std::enable_if_t<std::is_invocable_r_v<bool, InputFunc, std::string&>>>
	bool parse(InputFunc&& func)
	{
		return push_source(std::forward<InputFunc>(func)).parse();
	}

	bool parse()
	{
		detail::reentrancy_sentinel<reenterant_parse_error> const guard{parsing_};
		program const& prog = grammar_.program();
		if (prog.instructions.empty())
			throw bad_grammar{};
		environment_.start_parse(*this);
		detail::scope_exit const cleanup{[this]{ environment_.end_parse(); }};
		auto [sr, mr, rc, pc, fc] = drain();
		bool result = false, done = false;
		prune_depth_ = max_call_depth, call_depth_ = 0;
		pc = 0, fc = 0;
		while (!done) {
			auto [op, imm, off, str] = instruction::decode(prog.instructions, pc);
			switch (op) {
				case opcode::match: {
					if (!match_sequence(sr, str, [this](auto i, auto n, auto s) { return input_.compare(i, n, s) == 0; }))
						goto failure;
				} break;
				case opcode::match_casefold: {
					if (!match_sequence(sr, str, [this](auto i, auto n, auto s) { return casefold_compare(i, n, s) == 0; }))
						goto failure;
				} break;
				case opcode::match_any: {
					if (!match_single(sr, []{ return true; }))
						goto failure;
				} break;
				case opcode::match_any_of: {
					if (!match_single(sr, [pe = static_cast<unicode::property_enum>(imm), s = str](auto const& r) { return unicode::any_of(r, pe, s); }))
						goto failure;
				} break;
				case opcode::match_all_of: {
					if (!match_single(sr, [pe = static_cast<unicode::property_enum>(imm), s = str](auto const& r) { return unicode::all_of(r, pe, s); }))
						goto failure;
				} break;
				case opcode::match_none_of: {
					if (!match_single(sr, [pe = static_cast<unicode::property_enum>(imm), s = str](auto const& r) { return unicode::none_of(r, pe, s); }))
						goto failure;
				} break;
				case opcode::match_set: {
					if (!match_single(sr, [&runes = prog.runesets[imm]](char32_t rune) {
							auto const interval = std::lower_bound(runes.begin(), runes.end(), rune, [](auto& x, auto& y) { return x.second < y; });
							return interval != runes.end() && interval->first <= rune && rune <= interval->second; }))
						goto failure;
				} break;
				case opcode::match_eol: {
					if (!match_single(sr, [](auto curr, auto last, auto& next, char32_t rune) {
							if (curr == next || (unicode::query(rune).properties() & unicode::ptype::Line_Ending) == unicode::ptype::None)
								return false;
							if (U'\r' == rune)
								if (auto const [next2, rune2] = utf8::decode_rune(next, last); next2 != next && rune2 == U'\n')
									next = next2;
							return true; }))
						goto failure;
				} break;
				case opcode::choice: {
					stack_frames_.push_back(stack_frame_type::backtrack);
					backtrack_stack_.emplace_back(sr - imm, rc, pc + off);
				} break;
				case opcode::commit: {
					if (!commit<opcode::commit>(sr, rc, pc, off))
						goto failure;
				} break;
				case opcode::commit_back: {
					if (!commit<opcode::commit_back>(sr, rc, pc, off))
						goto failure;
				} break;
				case opcode::commit_partial: {
					if (!commit<opcode::commit_partial>(sr, rc, pc, off))
						goto failure;
				} break;
				case opcode::jump: {
					pc += off;
				} break;
				case opcode::call: {
					if (imm != 0) {
						auto const memo = detail::escaping_find_if(lrmemo_stack_.crbegin(), lrmemo_stack_.crend(),
								[sr = sr, pca = pc + off](auto const& m){ return m.srr == sr && m.pca == pca ? 1 : (m.srr < sr ? 0 : -1); });
						if (memo != lrmemo_stack_.crend()) {
							if (memo->sra == lrfailcode || imm < memo->prec)
								goto failure;
							sr = memo->sra, rc = restore_responses_after(rc, memo->responses);
							continue;
						}
						stack_frames_.push_back(stack_frame_type::lrcall);
						lrmemo_stack_.push_back({sr, lrfailcode, imm, pc, pc + off, rc, std::vector<semantic_response>{}});
					} else {
						stack_frames_.push_back(stack_frame_type::call);
						call_stack_.push_back(pc);
					}
					pc += off;
				} break;
				case opcode::ret: {
					if (stack_frames_.empty())
						goto failure;
					switch (stack_frames_.back()) {
						case stack_frame_type::call: {
							pc = call_stack_.back();
							pop_stack_frame(call_stack_);
						} break;
						case stack_frame_type::lrcall: {
							auto& memo = lrmemo_stack_.back();
							if (memo.sra == lrfailcode || sr > memo.sra) {
								memo.sra = sr, memo.responses = drop_responses_after(memo.rcr);
								sr = memo.srr, pc = memo.pca, rc = memo.rcr;
								continue;
							}
							sr = memo.sra, pc = memo.pcr, rc = restore_responses_after(memo.rcr, memo.responses);
							pop_stack_frame(lrmemo_stack_, sr, mr, rc, pc);
						} break;
						default: goto failure;
					}
				} break;
				case opcode::fail: {
					fc = imm;
				failure:
					for (mr = (std::max)(mr, sr), ++fc; fc > 0; --fc) {
						if (done = (cut_frame_ >= stack_frames_.size()); done) {
							registers_ = {sr, mr, rc, pc, 0};
							break;
						}
						switch (stack_frames_.back()) {
							case stack_frame_type::backtrack: {
								std::tie(sr, rc, pc) = backtrack_stack_.back();
								pop_stack_frame(backtrack_stack_);
							} break;
							case stack_frame_type::call: {
								pop_stack_frame(call_stack_), ++fc;
							} break;
							case stack_frame_type::capture: {
								pop_stack_frame(capture_stack_, sr, mr, rc, pc), ++fc;
							} break;
							case stack_frame_type::condition: {
								auto const& [cond_name, cond_value] = condition_stack_.back();
								environment_.set_condition(cond_name, cond_value);
								pop_stack_frame(condition_stack_), ++fc;
							} break;
							case stack_frame_type::lrcall: {
								if (auto const& memo = lrmemo_stack_.back(); memo.sra != lrfailcode)
									sr = memo.sra, pc = memo.pcr, rc = restore_responses_after(memo.rcr, memo.responses);
								else
									++fc;
								pop_stack_frame(lrmemo_stack_, sr, mr, rc, pc);
							} break;
							case stack_frame_type::symbol_definition: {
								pop_stack_frame(symbol_definition_stack_), ++fc;
							} break;
							case stack_frame_type::symbol_table: {
								environment_.symbols_.swap(symbol_table_stack_.back());
								pop_stack_frame(symbol_table_stack_), ++fc;
							} break;
							default: break;
						}
					}
					pop_responses_after(rc);
				} break;
				case opcode::accept: {
					if (cut_deferred_ = !capture_stack_.empty() || !lrmemo_stack_.empty(); !cut_deferred_) {
						accept(sr, mr, rc, pc);
						std::tie(sr, mr, rc, pc, std::ignore) = drain();
					}
				} break;
				case opcode::accept_final: {
					accept(sr, mr, rc, pc);
					result = done = true;
				} break;
				case opcode::action: {
					rc = push_response(call_stack_.size() + lrmemo_stack_.size(), imm);
				} break;
				case opcode::predicate: {
					registers_ = {sr, (std::max)(mr, sr), rc, pc, 0};
					bool const accepted = prog.predicates[imm](environment_);
					std::tie(sr, mr, rc, pc, fc) = registers_.as_tuple();
					pop_responses_after(rc);
					if (!accepted)
						goto failure;
				} break;
				case opcode::capture_start: {
					stack_frames_.push_back(stack_frame_type::capture);
					capture_stack_.push_back(static_cast<subject_location>(sr));
				} break;
				case opcode::capture_end: {
					if (stack_frames_.empty() || (stack_frames_.back() != stack_frame_type::capture))
						goto failure;
					auto const sr0 = static_cast<std::size_t>(capture_stack_.back()), sr1 = sr;
					pop_stack_frame(capture_stack_, sr, mr, rc, pc);
					if (sr0 > sr1)
						goto failure;
					rc = push_response(call_stack_.size() + lrmemo_stack_.size(), imm, {sr0, sr1 - sr0});
				} break;
				case opcode::condition_test: {
					if (environment_.has_condition(str) != (imm != 0))
						goto failure;
				} break;
				case opcode::condition_push: {
					stack_frames_.push_back(stack_frame_type::condition);
					condition_stack_.emplace_back(str, environment_.set_condition(str, imm != 0));
				} break;
				case opcode::condition_pop: {
					if (stack_frames_.empty() || (stack_frames_.back() != stack_frame_type::condition))
						goto failure;
					auto const& [cond_name, cond_value] = condition_stack_.back();
					environment_.set_condition(cond_name, cond_value);
					pop_stack_frame(condition_stack_);
				} break;
				case opcode::symbol_start: {
					stack_frames_.push_back(stack_frame_type::symbol_definition);
					symbol_definition_stack_.emplace_back(str, static_cast<subject_location>(sr));
				} break;
				case opcode::symbol_end: {
					if (stack_frames_.empty() || (stack_frames_.back() != stack_frame_type::symbol_definition))
						goto failure;
					auto const [symbol_name, symbol_sr] = symbol_definition_stack_.back();
					auto const sr0 = static_cast<std::size_t>(symbol_sr), sr1 = sr;
					pop_stack_frame(symbol_definition_stack_);
					if (sr0 > sr1)
						goto failure;
					environment_.add_symbol(symbol_name, std::string{match().substr(sr0, sr1 - sr0)});
				} break;
				case opcode::symbol_exists: {
					if (environment_.has_symbol(str) != (imm != 0))
						goto failure;
				} break;
				case opcode::symbol_all: {
					auto const& symbols = environment_.get_symbols(str);
					if (symbols.empty())
						goto failure;
					std::size_t tsr = sr;
					for (const auto& symbol : symbols)
						if (!match_sequence(tsr, symbol, [this](auto i, auto n, auto s) { return input_.compare(i, n, s) == 0; }))
							goto failure;
					sr = tsr;
				} break;
				case opcode::symbol_any: {
					auto const& symbols = environment_.get_symbols(str);
					if (symbols.empty())
						goto failure;
					bool matched = false;
					for (const auto& symbol : symbols) {
						if (match_sequence(sr, symbol, [this](auto i, auto n, auto s) { return input_.compare(i, n, s) == 0; })) {
							matched = true;
							break;
						}
					}
					if (!matched)
						goto failure;
				} break;
				case opcode::symbol_head: {
					auto const& symbols = environment_.get_symbols(str);
					if (imm >= symbols.size())
						goto failure;
					if (!match_sequence(sr, symbols[imm], [this](auto i, auto n, auto s) { return input_.compare(i, n, s) == 0; }))
						goto failure;
				} break;
				case opcode::symbol_tail: {
					auto const& symbols = environment_.get_symbols(str);
					if (imm >= symbols.size())
						goto failure;
					if (!match_sequence(sr, symbols[symbols.size() - imm - 1], [this](auto i, auto n, auto s) { return input_.compare(i, n, s) == 0; }))
						goto failure;
				} break;
				case opcode::symbol_push: {
					stack_frames_.push_back(stack_frame_type::symbol_table);
					symbol_table_stack_.emplace_back(environment_.symbols_);
					if (imm == 1)
						environment_.symbols_.erase(str);
					else if (imm == 2)
						environment_.symbols_.clear();
				} break;
				case opcode::symbol_pop: {
					if (stack_frames_.empty() || (stack_frames_.back() != stack_frame_type::symbol_table))
						goto failure;
					environment_.symbols_.swap(symbol_table_stack_.back());
					pop_stack_frame(symbol_table_stack_);
				} break;
				default: registers_ = {sr, (std::max)(mr, sr), rc, pc, 0}; throw bad_opcode{};
			}
		}
		return result;
	}
};

template <class InputIt, class = utf8::enable_if_char_input_iterator_t<InputIt>>
inline bool parse(InputIt first, InputIt last, grammar const& grmr, environment& envr)
{
	return parser{grmr, envr}.enqueue(first, last).parse();
}

template <class InputIt, class = utf8::enable_if_char_input_iterator_t<InputIt>>
inline bool parse(InputIt first, InputIt last, grammar const& grmr)
{
	environment envr;
	return parse(first, last, grmr, envr);
}

inline bool parse(std::istream& input, grammar const& grmr, environment& envr)
{
	return parser{grmr, envr}.push_source([&input](std::string& line) {
		if (std::getline(input, line)) {
			line.push_back('\n');
			return true;
		}
		return false;
	}).parse();
}

inline bool parse(std::istream& input, grammar const& grmr)
{
	environment envr;
	return parse(input, grmr, envr);
}

inline bool parse(std::string_view sv, grammar const& grmr, environment& envr)
{
	return parse(sv.cbegin(), sv.cend(), grmr, envr);
}

inline bool parse(std::string_view sv, grammar const& grmr)
{
	return parse(sv.cbegin(), sv.cend(), grmr);
}

inline bool parse(grammar const& grmr, environment& envr)
{
	return parse(std::cin, grmr, envr);
}

inline bool parse(grammar const& grmr)
{
	return parse(std::cin, grmr);
}

[[nodiscard]] inline std::string_view environment::match() const { return parser().match(); }
[[nodiscard]] inline syntax_position environment::position_at(std::size_t index) { return parser().position_at(index); }
[[nodiscard]] inline syntax_position environment::position_begin(syntax_range const& range) { return parser().position_begin(range); }
[[nodiscard]] inline syntax_position environment::position_end(syntax_range const& range) { return parser().position_end(range); }
[[nodiscard]] inline std::pair<syntax_position, syntax_position> environment::position_range(syntax_range const& range) { return parser().position_range(range); }
[[nodiscard]] inline unsigned short environment::call_depth() const { return parser().call_depth(); }
[[nodiscard]] inline unsigned short environment::prune_depth() const { return parser().prune_depth(); }
inline void environment::escape() { parser().escape(); }

LUG_DIAGNOSTIC_PUSH_AND_IGNORE

[[nodiscard]] inline grammar basic_regular_expression::make_grammar()
{
	using namespace language;
	implicit_space_rule default_space = nop;
	rule Empty = eps                                    <[](generator& g) { g.encoder.match_eps(); };
	rule Dot = chr('.')                                 <[](generator& g) { g.encoder.match_any(); };
	rule Element = any > chr('-') > !chr(']') > any     <[](generator& g, syntax const& x) { g.bracket_range(x.capture()); }
	    | str("[:") > +(!chr(':') > any) > str(":]")    <[](generator& g, syntax const& x) { g.bracket_class(x.capture().substr(2, x.range().size - 4)); }
	    | any                                           <[](generator& g, syntax const& x) { g.bracket_range(x.capture(), x.capture()); };
	rule Bracket = chr('[') > ~(chr('^')                <[](generator& g) { g.circumflex = true; })
	    > Element > *(!chr(']') > Element) > chr(']')   <[](generator& g) { g.bracket_commit(); };
	rule Sequence = +(!(chr('.') | chr('[')) > any)     <[](generator& g, syntax const& x) { g.encoder.match(x.capture()); };
	return start((+(Dot | Bracket | Sequence) | Empty) > eoi);
}

LUG_DIAGNOSTIC_POP

inline void basic_regular_expression::operator()(encoder& d) const
{
	if (program_->instructions.empty()) {
		static grammar const grmr = make_grammar();
		generator genr(*this, d.mode() & directives::caseless);
		if (!parse(expression_, grmr, genr))
			throw bad_string_expression{};
	}
	d.skip((program_->mandate & directives::eps) ^ directives::eps).append(*program_);
}

} // namespace lug

#endif
