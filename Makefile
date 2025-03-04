# lug - Embedded DSL for PE grammar parsers in C++
# See LICENSE file for copyright and license details

# distribution version
VERSION = 0.4.0

# paths
PREFIX = /usr/local

# toolchain
CXXSTD = -std=c++17
CXXFLAGS = $(CXXSTD) -pedantic -Wall -Wconversion -Wextra -Wextra-semi -Wshadow -Wsign-conversion -Wsuggest-override -Wno-parentheses -Wno-logical-not-parentheses \
			-Os -ffunction-sections -fdata-sections -I.
LDFLAGS = $(CXXSTD) -s
CLANGTIDY = clang-tidy

# unicode character database version
UCD_VERSION = 15.1.0

# samples
SAMPLES = basic/basic calc/calc demo/demo json/json_matcher json/json_parser xml/xml
SAMPLES_BIN = $(SAMPLES:%=samples/%)
SAMPLES_OBJ = $(SAMPLES:%=samples/%.o)

# tests
TESTS = acceptcut attributes captures conditions errorhandling leftrecursion nonterminals parser predicates symbols terminals
TESTS_BIN = $(TESTS:%=tests/%)
TESTS_OBJ = $(TESTS:%=tests/%.o)

# tools
TOOLS = makeunicode
TOOLS_BIN = $(TOOLS:%=tools/%)
TOOLS_OBJ = $(TOOLS:%=tools/%.o)

# header dependencies
HEADERS = lug/detail.hpp lug/error.hpp lug/unicode.hpp lug/utf8.hpp lug/lug.hpp

# distribution files
DISTFILES = CHANGELOG.md LICENSE.md README.md CMakeLists.txt Makefile runtests.sh .clang-tidy .editorconfig .gitattributes .gitignore .github/ doc/ lug/ samples/ tests/ tools/

all: options samples tests

.cpp.o:
	@echo CXX $<
	@$(CXX) -c $(CXXFLAGS) $$(if [ "$(CI_BUILD)" = "1" ]; then echo "-Werror"; fi) -o $@ $<

$(SAMPLES_OBJ): $(HEADERS)

$(SAMPLES_BIN): $(SAMPLES_OBJ)
	@echo LD $@
	@$(CXX) -o $@ $@.o $(LDFLAGS)

samples: $(SAMPLES_BIN)

$(TESTS_OBJ): $(HEADERS)

$(TESTS_BIN): $(TESTS_OBJ)
	@echo LD $@
	@$(CXX) -o $@ $@.o $(LDFLAGS)

tests: $(TESTS_BIN)

check: tests
	@sh runtests.sh "tests" $(TESTS_BIN)

lint:
	@$(CLANGTIDY) --quiet $(CXXFLAGS:%=--extra-arg=%) $(HEADERS)

$(TOOLS_OBJ): $(HEADERS)

$(TOOLS_BIN): $(TOOLS_OBJ)
	@echo LD $@
	@$(CXX) -o $@ $@.o $(LDFLAGS)

tools: $(TOOLS_BIN)

unicode: tools
	@echo fetching Unicode Character Database $(UCD_VERSION)
	@cd tools/ && sh fetchucd.sh $(UCD_VERSION)
	@echo generating lug/unicode.hpp
	@cd tools/ && ./makeunicode > ../lug/unicode.hpp

options:
	@echo lug build options:
	@echo "CXX         = $(CXX)"
	@echo "CXXSTD      = $(CXXSTD)"
	@echo "CXXFLAGS    = $(CXXFLAGS)"
	@echo "LDFLAGS     = $(LDFLAGS)"
	@echo "CLANGTIDY   = $(CLANGTIDY)"
	@echo "PREFIX      = $(PREFIX)"
	@echo "UCD_VERSION = $(UCD_VERSION)"

clean:
	@echo cleaning
	@rm -f $(SAMPLES_BIN) $(SAMPLES_OBJ) $(TESTS_BIN) $(TESTS_OBJ) $(TOOLS_BIN) $(TOOLS_OBJ) lug-$(VERSION).tar.gz
	@rm -rf tools/ucd

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

.PHONY: all samples tests check lint tools unicode options clean dist install uninstall
