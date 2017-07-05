// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017 Jesse W. Towner
// See LICENSE.md file for license details

// Derived from BASIC, Dartmouth College Computation Center, October 1st 1964
// http://www.bitsavers.org/pdf/dartmouth/BASIC_Oct64.pdf

#include <lug.hpp>
#include <cmath>
#include <cstdlib>
#include <iomanip>
#include <map>

#ifdef _MSC_VER
#include <io.h>
#define fileno _fileno
#define isatty _isatty
#else
#include <unistd.h>
#endif

class basic_interpreter
{
public:
	basic_interpreter()
	{
		using namespace lug::language;

		rule Expr;

		rule _		= *"[ \t]"s;
		rule CR		= "\n"s | "\r\n" | "\r";
		rule Var	= id_<< ("[A-Z]" > ~"[0-9]"s) > _               <[this]{ return *id_; };
		rule Func	= id_<< ("[A-Z]" > *"[A-Z0-9]"s) > _            <[this]{ return *id_; };
		rule LineNo	= sv_<< +"[0-9]"s > _                           <[this]{ return std::stoi(std::string{*sv_}); };
		rule Real	= sv_<< (+"[0-9]"s > ~("[.]"s > +"[0-9]"s)
					> ~("[eE]"s > ~"[+-]"s > +"[0-9]"s)) > _        <[this]{ return std::stod(std::string{*sv_}); };

		rule String	= "\"" > sv_<< *"[^\"]"s > "\"" > _             <[this]{ return *sv_; };

		rule RelOp	= "=" > _                                       <[]()->RelOpFn{ return [](double x, double y) { return x == y; }; }
					| ">=" > _                                      <[]()->RelOpFn{ return std::isgreaterequal; }
					| ">" > _                                       <[]()->RelOpFn{ return std::isgreater; }
					| "<=" > _                                      <[]()->RelOpFn{ return std::islessequal; }
					| "<>" > _                                      <[]()->RelOpFn{ return [](double x, double y) { return x != y; }; }
					| "<" > _                                       <[]()->RelOpFn{ return std::isless; };

		rule Factor	= id_%Func > "(" > _ > r1_%Expr > ")" > _       <[this]{ return fn(*id_, *r1_); }
					| id_%Var                                       <[this]{ return vars_[*id_]; }
					| r1_%Real > ~(u8"[↑^]"s > _ > r2_%Real         <[this]{ *r1_ = std::pow(*r1_, *r2_); }
					)                                               <[this]{ return *r1_; }
					| "(" > _ > Expr > ")" > _;

		rule Term	= r1_%Factor > *(
					      "*" > _ > r2_%Factor                      <[this]{ *r1_ *= *r2_; }
					    | "/" > _ > r2_%Factor                      <[this]{ *r1_ /= *r2_; }
					)                                               <[this]{ return *r1_; };

		     Expr	= (  ~ "+"s > _ > r1_%Term
					     | "-"  > _ > r1_%Term                      <[this]{ *r1_ = -*r1_; }
					) > *( "+"  > _ > r2_%Term                      <[this]{ *r1_ += *r2_; }
					     | "-"  > _ > r2_%Term                      <[this]{ *r1_ -= *r2_; }
					)                                               <[this]{ return *r1_; };

		rule InpLst	= id_%Var                                       <[this]{ std::cin >> vars_[*id_]; }
					> *( "," > _ > id_%Var                          <[this]{ std::cin >> vars_[*id_]; } );

		rule PrtTok	= sv_%String                                    <[this]{ std::cout << *sv_; }
					| r1_%Expr                                      <[this]{ std::cout << *r1_; };

		rule Stmnt	= "PRINT" > _ > ~PrtTok > *("," > _ > PrtTok)   <[this]{ std::cout << std::endl; }
					| "IF" > _ > r1_%Expr > rop_%RelOp > r2_%Expr   <[this]{ if (!(*rop_)(*r1_, *r2_)) { semantics_.escape(); } }
					    > "THEN" > _ > Stmnt
					| "FOR" > _ > id_%Var > "=" > _ > r1_%Expr
					    > "TO" > _ > r2_%Expr
					    > ( "STEP" > _ > r3_%Expr
					      | "" < [this]{ *r3_ = 1.0; })             <[this]{ for_to_step(*id_, *r1_, *r2_, *r3_); }
					| "NEXT" > _ > id_%Var                          <[this]{ next(*id_); }
					| "GOTO" > _ > no_%LineNo                       <[this]{ goto_line(*no_); }
					| "INPUT" > _ > InpLst
					| "LET" > _ > id_%Var > "=" > _ > r1_%Expr      <[this]{ vars_[*id_] = *r1_; }
					| "GOSUB" > _ > no_%LineNo                      <[this]{ gosub(*no_); }
					| "RETURN" > _                                  <[this]{ retsub(); }
					| "CLEAR" > _                                   <[this]{ lines_.clear(); }
					| "LIST" > _                                    <[this]{ listing(); }
					| "RUN" > _                                     <[this]{ line_ = lines_.begin(); }
					| ("END"s | "STOP") > _                         <[this]{ line_ = lines_.end(); }
					| ("EXIT"s | "QUIT") > _                        <[this]{ std::exit(EXIT_SUCCESS); }
					| "REM" > _ > *(!CR > ".");

		rule Line	= _ > Stmnt > CR
					| _ > no_%LineNo
					    > sv_<< (*(!CR > ".") > CR)                 <[this]{ update_line(*no_, *sv_); }
					| _ > CR
					| _ > (*(!CR > ".") > CR)                       <[this]{ print_error(line_, "ILLEGAL FORMULA"); }
					| _ > !"."s                                     <[this]{ std::exit(EXIT_SUCCESS); };

		grammar_ = start(Line);
	}

	void repl()
	{
		lug::parser parser{grammar_, semantics_};

		parser.push_source([this](std::string& out) {
			if (line_ != lines_.end()) {
				lastline_ = line_++;
				out = lastline_->second;
			} else {
				if (stdin_tty_) {
					std::cout << "> ";
					std::cout.flush();
				}
				if (!std::getline(std::cin, out))
					return false;
				out.push_back('\n');
			}
			return true;
		});

		while (parser.parse()) ;
	}

private:
	void print_error(std::map<int, std::string>::iterator line, const char* message)
	{
		std::cerr << message << "\n";
		if (line != lines_.end())
			std::cerr << "LINE " << line->first << ": " << line->second;
		std::cerr.flush();
		line_ = lastline_ = lines_.end();
		stack_.clear(), for_stack_.clear();
	}

	void listing()
	{
		for (const auto& [n, l] : lines_)
			std::cout << std::left << std::setw(7) << n << " " << l;
		std::cout.flush();
	}

	void update_line(int n, std::string_view s)
	{
		if (s.empty() || s.front() < ' ')
			lines_.erase(n);
		else
			lines_[n] = s;
	}

	bool goto_line(int n)
	{
		if (lastline_ = line_, line_ = lines_.find(n); line_ == lines_.end()) {
			print_error(lastline_, "ILLEGAL LINE NUMBER");
			return false;
		}
		return true;
	}

	void gosub(int n)
	{
		lastline_ = line_;
		if (goto_line(n))
			stack_.push_back(lastline_);
	}

	void retsub()
	{
		if (!stack_.empty())
			line_ = stack_.back(), stack_.pop_back();
		else
			print_error(line_, "ILLEGAL RETURN");
	}

	void for_to_step(const std::string& id, double from, double to, double step)
	{
		if (lastline_ != lines_.end()) {
			double& v = vars_[id];
			if (!for_stack_.empty() && id == for_stack_.back().first) {
				v += step;
			} else {
				for_stack_.emplace_back(id, lastline_);
				v = from;
			}
			if ((step >= 0 && v < to) || (step < 0 && v > to))
				return;
			for_stack_.pop_back();
			for ( ; line_ != lines_.end(); ++line_) {
				if (auto& t = line_->second; t.compare(0, 4, "NEXT") == 0) {
					if (auto i = t.find_first_not_of(" \t", 4); i != std::string::npos && t.compare(i, id.size(), id) == 0) {
						lastline_ = line_++;
						return;
					}
				}
			}
		}
		print_error(line_, "FOR WITHOUT NEXT");
	}

	void next(const std::string& id)
	{
		if (lastline_ != lines_.end() && !for_stack_.empty() && for_stack_.back().first == id) {
			lastline_ = line_;
			line_ = for_stack_.back().second;
		} else {
			print_error(line_, "NOT MATCH WITH FOR");
		}
	}

	double fn(const std::string& id, double x)
	{
		using namespace std::literals::string_literals;
		static const std::unordered_map<std::string, double(*)(double)> intrinsics{
			{"SIN"s, std::sin}, {"COS"s, std::cos}, {"TAN"s, std::tan}, {"ATN"s, std::atan},
			{"EXP"s, std::exp}, {"ABS"s, std::abs}, {"LOG"s, std::log}, {"SQR"s, std::sqrt},
			{"INT"s, std::trunc}, {"RND"s, [](double){ return std::rand() / static_cast<double>(RAND_MAX); }}
		};
		if (auto intrinsic = intrinsics.find(id); intrinsic != intrinsics.end())
			return intrinsic->second(x);
		print_error(line_, "UNDEFINED FUNCTION");
		return std::nan(nullptr);
	}

	using RelOpFn = bool(*)(double, double);
	lug::grammar grammar_;
	lug::semantics semantics_;
	lug::variable<std::string> id_{semantics_};
	lug::variable<std::string_view> sv_{semantics_};
	lug::variable<double> r1_{semantics_}, r2_{semantics_}, r3_{semantics_};
	lug::variable<int> no_{semantics_};
	lug::variable<RelOpFn> rop_{semantics_};
	std::unordered_map<std::string, double> vars_;
	std::map<int, std::string> lines_;
	std::map<int, std::string>::iterator line_{lines_.end()}, lastline_{lines_.end()};
	std::vector<std::map<int, std::string>::iterator> stack_;
	std::vector<std::pair<std::string, std::map<int, std::string>::iterator>> for_stack_;
	const bool stdin_tty_{isatty(fileno(stdin)) != 0};
};

int main()
{
	try {
		basic_interpreter interpreter;
		interpreter.repl();
	} catch (std::exception& e) {
		std::cerr << "Error: " << e.what() << std::endl;
		return -1;
	}
	return 0;
}
