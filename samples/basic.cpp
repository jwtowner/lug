// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>
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
	basic_interpreter() : line_{lines_.end()}, stdin_tty_{isatty(fileno(stdin)) != 0}
	{
		using namespace lug::language;

		rule Expr;

		rule _		= *"[ \t]"s;
		rule S		= +"[ \t]"s;
		rule CR		= "\n"s | "\r\n" | "\r";
		rule Ident	= id_<< +"[A-Z]" > _                            <[this]{ return id_; };
		rule Number	= sv_<< (+"[0-9]"s) > _                         <[this]{ return std::stoi(std::string{sv_}); };
		rule String	= "\"" > sv_<< *"[^\"]"s > "\"" > _             <[this]{ return sv_; };

		rule RelOp	= "=" > _                                       <[]() -> RelOpFn { return [](int x, int y) { return x == y; }; }
					| ">=" > _                                      <[]() -> RelOpFn { return [](int x, int y) { return x >= y; }; }
					| ">" > _                                       <[]() -> RelOpFn { return [](int x, int y) { return x > y; }; }
					| "<=" > _                                      <[]() -> RelOpFn { return [](int x, int y) { return x <= y; }; }
					| "<>" > _                                      <[]() -> RelOpFn { return [](int x, int y) { return x != y; }; }
					| "<" > _                                       <[]() -> RelOpFn { return [](int x, int y) { return x < y; }; };

		rule Factor	= id_%Ident                                     <[this]{ return vars_[id_]; }
					| Number
					| "(" > _ > Expr > ")" > _;

		rule Term	= n1_%Factor > *(
					      "*" > _ > n2_%Factor                      <[this]{ n1_ *= n2_; }
					    | "/" > _ > n2_%Factor                      <[this]{ n1_ /= n2_; }
					)                                               <[this]{ return n1_; };

		     Expr	= (  ~ "+"s > _ > n1_%Term
					     | "-"  > _ > n1_%Term                      <[this]{ n1_ = -n1_; }
					) > *( "+"  > _ > n2_%Term                      <[this]{ n1_ += n2_; }
					     | "-"  > _ > n2_%Term                      <[this]{ n1_ -= n2_; }
					)                                               <[this]{ return n1_; };

		rule InpLst	= id_%Ident                                     <[this]{ std::cin >> vars_[id_]; }
					> *( "," > _ > id_%Ident                        <[this]{ std::cin >> vars_[id_]; } );

		rule PrtTok	= sv_%String                                    <[this]{ std::cout << sv_; }
					| n1_%Expr                                      <[this]{ std::cout << n1_; };

		rule Stmnt	= "PRINT" > S > ~PrtTok > *("," > _ > PrtTok)   <[this]{ std::cout << std::endl; }
					| "IF" > S > n1_%Expr > rop_%RelOp > n2_%Expr   <[this]{ if (!rop_(n1_, n2_)) { /*SKIP STATEMENT*/ } } > "THEN" > S > Stmnt
					| "GOTO" > S > n1_%Expr                         <[this]{ gotoline(n1_); }
					| "INPUT" > S > InpLst
					| "LET" > S > id_%Ident > "=" > _ > n1_%Expr    <[this]{ vars_[id_] = n1_; }
					| "GOSUB" > S > n1_%Expr                        <[this]{ gosub(n1_); }
					| "RETURN" > _                                  <[this]{ retsub(); }
					| "CLEAR" > _                                   <[this]{ lines_.clear(); }
					| "LIST" > _                                    <[this]{ prlines(); }
					| "RUN" > _                                     <[this]{ line_ = lines_.begin(); }
					| "END" > _                                     <[this]{ line_ = lines_.end(); }
					| "STOP" > _                                    <[this]{ std::exit(EXIT_SUCCESS); }
					| "REM" > _ > *(!CR > ".");

		rule Line	= _ > Stmnt > CR
					| _ > n1_%Number
					    > sv_<< (*(!CR > ".") > CR)                 <[this]{ updateline(n1_, sv_); }
					| _ > CR
					| _ > (*(!CR > ".") > CR)                       <[this]{ prerror(line_, "syntax error"); }
					| _ > !"."s                                     <[this]{ std::exit(EXIT_SUCCESS); };

		grammar_ = start(Line);
	}

	void repl()
	{
		lug::semantic_environment sema;
		lug::parser parser{grammar_, sema};

		parser.push_source( [this](std::string& out) {
			if (line_ != lines_.end()) {
				out = (line_++)->second;
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
	void prerror(std::map<int, std::string>::iterator line, const char* message)
	{
		if (line != lines_.end())
			std::cerr << "\nline " << line->first << ": " << line->second << "\n";
		std::cerr << message << std::endl;
		line_ = lines_.end();
	}

	void prlines()
	{
		for (const auto&[n, l] : lines_)
			std::cout << std::setw(6) << n << l << "\n";
		std::cout.flush();
	}

	void updateline(int n, std::string_view s)
	{
		if (s.empty() || s.front() < ' ')
			lines_.erase(n);
		else
			lines_[n] = s;
	}

	bool gotoline(int n)
	{
		auto prevline = line_;
		line_ = lines_.find(n);
		if (line_ == lines_.end()) {
			prerror(prevline, "invalid line number");
			return false;
		}
		return true;
	}

	void gosub(int n)
	{
		auto prevline = line_;
		if (gotoline(n))
			stack_.push_back(prevline);
	}

	void retsub()
	{
		if (!stack_.empty())
			line_ = stack_.back(), stack_.pop_back();
		else
			prerror(line_, "missing stack frame");
	}

	using RelOpFn = bool(*)(int, int);

	lug::grammar grammar_;
	std::string id_;
	std::string_view sv_;
	int n1_, n2_;
	RelOpFn rop_;
	std::unordered_map<std::string, int> vars_;
	std::map<int, std::string> lines_;
	std::map<int, std::string>::iterator line_;
	std::vector<std::map<int, std::string>::iterator> stack_;
	bool stdin_tty_;
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
