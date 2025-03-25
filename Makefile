# lug - Embedded DSL for PE grammar parsers in C++
# Copyright (c) 2017-2025 Jesse W. Towner
# See LICENSE file for copyright and license details

# This is a POSIX.1-2014 compliant Makefile. It is known to work with GNU Make, NetBSD Make and PDP Make.
# https://pubs.opengroup.org/onlinepubs/9799919799/utilities/make.html
.POSIX:
.PHONY: all clean dist install uninstall options test units rununits samples runsamples lint clangtidy shellcheck tools unicode
.SUFFIXES: .cpp .o

# distribution version
VERSION = 0.6.0

# unicode character database version
UCD_VERSION = 16.0.0

# paths
PREFIX = /usr/local

# toolchain
CXX = c++
CXXSTD = -std=c++17
CXXWARNFLAGS = -pedantic -Wall -Wconversion -Wextra -Wextra-semi -Wshadow -Wsign-conversion -Wsuggest-override -Wno-parentheses -Wno-logical-not-parentheses
CXXOPTFLAGS = -Os -ffunction-sections -fdata-sections
CXXFLAGS = $(CXXSTD) $(CXXWARNFLAGS) $(CXXOPTFLAGS) -Iinclude
LDFLAGS = $(CXXSTD) -s
CLANGTIDY = clang-tidy
SHELLCHECK = shellcheck

# samples
SAMPLES = basic/basic calc/calc demo/demo json/jsoncheck json/jsonformat xml/xmlcheck
SAMPLES_BIN = $(SAMPLES:%=samples/%)
SAMPLES_OBJ = $(SAMPLES:%=samples/%.o)

# sample tests
SAMPLES_TESTS_DIRS = basic calc demo json xml
SAMPLES_TESTPLANS = $(SAMPLES_TESTS_DIRS:%=samples/%/.testplan)

# unit tests
TESTS = acceptcut attributes captures conditions encoding errorhandling leftrecursion nonterminals parser predicates symbols terminals
TESTS_BIN = $(TESTS:%=tests/%)
TESTS_OBJ = $(TESTS:%=tests/%.o)

# tools
TOOLS = makeunicode
TOOLS_BIN = $(TOOLS:%=tools/%)
TOOLS_OBJ = $(TOOLS:%=tools/%.o)

# header dependencies
HEADER_NAMES = detail error iostream unicode utf8 lug
HEADERS = $(HEADER_NAMES:%=include/lug/%.hpp)

# shell scripts
SHELLSCRIPTS = runsamples.sh runtests.sh tools/fetchucd.sh

# distribution files
DISTDIRS = .github/ doc/ include/ samples/ tests/ tools/
DISTDOCFILES = CHANGELOG.md LICENSE.md README.md
DISTPROJFILES = CMakeLists.txt Makefile runsamples.sh runtests.sh .clang-tidy .editorconfig .gitattributes .gitignore
DISTFILES = $(DISTDOCFILES) $(DISTPROJFILES) $(DISTDIRS)

all: options units samples

.cpp.o:
	@echo CXX $<
	@$(CXX) -c $(CXXFLAGS) $$(if [ "$(CI_BUILD)" = "1" ]; then echo "-Werror"; fi) -o $@ $<

$(SAMPLES_OBJ): $(HEADERS)

$(SAMPLES_BIN): $(SAMPLES_OBJ)
	@echo LD $@
	@$(CXX) -o $@ $@.o $(LDFLAGS)

samples: $(SAMPLES_BIN)

runsamples: samples $(SAMPLES_TESTPLANS)
	@sh runsamples.sh $(SAMPLES_TESTPLANS)

$(TESTS_OBJ): $(HEADERS)

$(TESTS_BIN): $(TESTS_OBJ)
	@echo LD $@
	@$(CXX) -o $@ $@.o $(LDFLAGS)

units: $(TESTS_BIN)

rununits: units
	@sh runtests.sh $(TESTS_BIN)

test: units samples
	@sh runtests.sh $(TESTS_BIN)
	@echo
	@sh runsamples.sh $(SAMPLES_TESTPLANS)

clangtidy:
	@$(CLANGTIDY) --quiet $(CXXFLAGS:%=--extra-arg=%) $(HEADERS)

shellcheck:
	@$(SHELLCHECK) -s sh $(SHELLSCRIPTS)

lint: clangtidy shellcheck

$(TOOLS_OBJ): $(HEADERS)

$(TOOLS_BIN): $(TOOLS_OBJ)
	@echo LD $@
	@$(CXX) -o $@ $@.o $(LDFLAGS)

tools: $(TOOLS_BIN)

unicode: tools
	@echo fetching Unicode Character Database $(UCD_VERSION)
	@cd tools/ && sh fetchucd.sh $(UCD_VERSION)
	@echo generating include/lug/unicode.hpp
	@cd tools/ && ./makeunicode > ../include/lug/unicode.hpp

options:
	@echo lug build options:
	@echo "CXX          = $(CXX)"
	@echo "CXXSTD       = $(CXXSTD)"
	@echo "CXXWARNFLAGS = $(CXXWARNFLAGS)"
	@echo "CXXOPTFLAGS  = $(CXXOPTFLAGS)"
	@echo "CXXFLAGS     = $(CXXFLAGS)"
	@echo "LDFLAGS      = $(LDFLAGS)"
	@echo "CLANGTIDY    = $(CLANGTIDY)"
	@echo "SHELLCHECK   = $(SHELLCHECK)"
	@echo "PREFIX       = $(PREFIX)"
	@echo "UCD_VERSION  = $(UCD_VERSION)"

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
	@cp -f include/lug/lug.hpp $(DESTDIR)$(PREFIX)/include/lug
	@chmod 644 $(DESTDIR)$(PREFIX)/include/lug/lug.hpp
	@cp -f include/lug/detail.hpp $(DESTDIR)$(PREFIX)/include/lug
	@chmod 644 $(DESTDIR)$(PREFIX)/include/lug/detail.hpp
	@cp -f include/lug/error.hpp $(DESTDIR)$(PREFIX)/include/lug
	@chmod 644 $(DESTDIR)$(PREFIX)/include/lug/error.hpp
	@cp -f include/lug/iostream.hpp $(DESTDIR)$(PREFIX)/include/lug
	@chmod 644 $(DESTDIR)$(PREFIX)/include/lug/iostream.hpp
	@cp -f include/lug/unicode.hpp $(DESTDIR)$(PREFIX)/include/lug
	@chmod 644 $(DESTDIR)$(PREFIX)/include/lug/unicode.hpp
	@cp -f include/lug/utf8.hpp $(DESTDIR)$(PREFIX)/include/lug
	@chmod 644 $(DESTDIR)$(PREFIX)/include/lug/utf8.hpp

uninstall:
	@echo removing header files from $(DESTDIR)$(PREFIX)/include/lug
	@rm -f $(DESTDIR)$(PREFIX)/include/lug/lug.hpp
	@rm -f $(DESTDIR)$(PREFIX)/include/lug/detail.hpp
	@rm -f $(DESTDIR)$(PREFIX)/include/lug/error.hpp
	@rm -f $(DESTDIR)$(PREFIX)/include/lug/iostream.hpp
	@rm -f $(DESTDIR)$(PREFIX)/include/lug/unicode.hpp
	@rm -f $(DESTDIR)$(PREFIX)/include/lug/utf8.hpp
	@rmdir $(DESTDIR)$(PREFIX)/include/lug
