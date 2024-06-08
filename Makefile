# lug - Embedded DSL for PE grammar parsers in C++
# See LICENSE file for copyright and license details

# distribution version
VERSION = 0.2-pre

# paths
PREFIX = /usr/local

# toolchain
CXXSTD = -std=c++17
CXXFLAGS = $(CXXSTD) -pedantic -Wall -Wextra -Wextra-semi -Wsign-conversion -Wsuggest-override -Wno-parentheses -Wno-logical-not-parentheses -Os -I.
LDFLAGS = $(CXXSTD) -s

# samples
SAMPLES = basic calc json
SAMPLES_BIN = $(SAMPLES:%=samples/%)
SAMPLES_OBJ = $(SAMPLES:%=samples/%.o)

# tests
TESTS = leftrecursion nonterminals parser predicates terminals
TESTS_BIN = $(TESTS:%=tests/%)
TESTS_OBJ = $(TESTS:%=tests/%.o)

# tools
TOOLS = makeunicode
TOOLS_BIN = $(TOOLS:%=tools/%)
TOOLS_OBJ = $(TOOLS:%=tools/%.o)

# dependencies
DEPS = lug/lug.hpp lug/detail.hpp lug/error.hpp lug/unicode.hpp lug/utf8.hpp

# distribution files
DISTFILES = README.md LICENSE.md Makefile lug.sln runtests.sh doc/ lug/ msvs/ samples/ tests/ tools/ 

all: options samples tests

.cpp.o:
	@echo CXX $<
	@$(CXX) -c $(CXXFLAGS) -o $@ $<

$(SAMPLES_OBJ): $(DEPS)

$(SAMPLES_BIN): $(SAMPLES_OBJ)
	@echo LD $@
	@$(CXX) -o $@ $@.o $(LDFLAGS)

samples: $(SAMPLES_BIN)

$(TESTS_OBJ): $(DEPS)

$(TESTS_BIN): $(TESTS_OBJ)
	@echo LD $@
	@$(CXX) -o $@ $@.o $(LDFLAGS)

tests: $(TESTS_BIN)

$(TOOLS_OBJ): $(DEPS)

$(TOOLS_BIN): $(TOOLS_OBJ)
	@echo LD $@
	@$(CXX) -o $@ $@.o $(LDFLAGS)

tools: $(TOOLS_BIN)

check: tests
	@sh runtests.sh "tests" $(TESTS_BIN)

options:
	@echo lug build options:
	@echo "CXX       = $(CXX)"
	@echo "CXXSTD    = $(CXXSTD)"
	@echo "CXXFLAGS  = $(CXXFLAGS)"
	@echo "LDFLAGS   = $(LDFLAGS)"

clean:
	@echo cleaning
	@rm -f $(SAMPLES_BIN) $(SAMPLES_OBJ) $(TESTS_BIN) $(TESTS_OBJ) $(TOOLS_BIN) $(TOOLS_OBJ) lug-$(VERSION).tar.gz

dist: clean
	@echo creating dist tarball
	@mkdir -p lug-$(VERSION)
	@cp -R $(DISTFILES) lug-$(VERSION)
	@tar -cf lug-$(VERSION).tar lug-$(VERSION)
	@gzip lug-$(VERSION).tar
	@rm -rf lug-$(VERSION)

install: all
	@echo installing header file to $(DESTDIR)$(PREFIX)/include/lug
	@mkdir -p $(DESTDIR)$(PREFIX)/include/lug
	@cp -f lug/lug.hpp $(DESTDIR)$(PREFIX)/include/lug
	@chmod 644 $(DESTDIR)$(PREFIX)/include/lug/lug.hpp
	@cp -f lug/detail.hpp $(DESTDIR)$(PREFIX)/include/lug
	@chmod 644 $(DESTDIR)$(PREFIX)/include/lug/detail.hpp
	@cp -f lug/error.hpp $(DESTDIR)$(PREFIX)/include/lug
	@chmod 644 $(DESTDIR)$(PREFIX)/include/lug/error.hpp
	@cp -f lug/unicode.hpp $(DESTDIR)$(PREFIX)/include/lug
	@chmod 644 $(DESTDIR)$(PREFIX)/include/lug/unicode.hpp
	@cp -f lug/utf8.hpp $(DESTDIR)$(PREFIX)/include/lug
	@chmod 644 $(DESTDIR)$(PREFIX)/include/lug/utf8.hpp

uninstall:
	@echo removing header files from $(DESTDIR)$(PREFIX)/include/lug
	@rm -f $(DESTDIR)$(PREFIX)/include/lug/lug.hpp
	@rm -f $(DESTDIR)$(PREFIX)/include/lug/detail.hpp
	@rm -f $(DESTDIR)$(PREFIX)/include/lug/error.hpp
	@rm -f $(DESTDIR)$(PREFIX)/include/lug/unicode.hpp
	@rm -f $(DESTDIR)$(PREFIX)/include/lug/utf8.hpp
	@rmdir $(DESTDIR)$(PREFIX)/include/lug

.PHONY: all check options samples tests tools clean dist install uninstall
