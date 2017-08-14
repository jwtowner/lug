# lug - Embedded DSL for PE grammar parsers in C++
# See LICENSE file for copyright and license details

# distribution version
VERSION = 0.1

# paths
PREFIX = /usr/local

# toolchain
CXX = g++
CXXSTD = -std=c++17
CXXFLAGS = $(CXXSTD) -pedantic -Wall -Os -I.
LDFLAGS = $(CXXSTD) -s

# samples
SAMPLES = basic calc json
SAMPLES_BIN = $(SAMPLES:%=samples/%)
SAMPLES_OBJ = $(SAMPLES:%=samples/%.o)

# tests
TESTS = leftrecursion predicates terminals
TESTS_BIN = $(SAMPLES:%=tests/%)
TESTS_OBJ = $(SAMPLES:%=tests/%.o)

# dependencies
DEPS = lug/lug.hpp lug/unicode.hpp lug/utf8.hpp

all: options samples tests

.cpp.o:
	@echo CXX $<
	@$(CXX) -c $(CXXFLAGS) -o $@ $<

$(SAMPLES_OBJ): $(DEPS)

$(SAMPLES_BIN): $(SAMPLES_OBJ)
	@echo CXX -o $@
	@$(CXX) -o $@ $@.o $(LDFLAGS)

samples: $(SAMPLES_BIN)

$(TESTS_OBJ): $(DEPS)

$(TESTS_BIN): $(TESTS_OBJ)
	@echo CXX -o $@
	@$(CXX) -o $@ $@.o $(LDFLAGS)

tests: $(TESTS_BIN)

options:
	@echo lug build options:
	@echo "CXX       = $(CXX)"
	@echo "CXXSTD    = $(CXXSTD)"
	@echo "CXXLAGS   = $(CXXFLAGS)"
	@echo "LDFLAGS   = $(LDFLAGS)"

clean:
	@echo cleaning
	@rm -f $(SAMPLES_BIN) $(SAMPLES_OBJ) $(TESTS_BIN) $(TESTS_OBJ) lug-$(VERSION).tar.gz

dist: clean
	@echo creating dist tarball
	@mkdir -p lug-$(VERSION)
	@cp -R README.md LICENSE.md Makefile lug.sln lug/ msvs/ samples/ tests/ unicode/ lug-$(VERSION)
	@tar -cf lug-$(VERSION).tar lug-$(VERSION)
	@gzip lug-$(VERSION).tar
	@rm -rf lug-$(VERSION)

install: all
	@echo installing header file to $(DESTDIR)$(PREFIX)/include/lug
	@mkdir -p $(DESTDIR)$(PREFIX)/include/lug
	@cp -f lug/lug.hpp $(DESTDIR)$(PREFIX)/include/lug
	@chmod 644 $(DESTDIR)$(PREFIX)/include/lug/lug.hpp
	@cp -f lug/unicode.hpp $(DESTDIR)$(PREFIX)/include/lug
	@chmod 644 $(DESTDIR)$(PREFIX)/include/lug/unicode.hpp
	@cp -f lug/utf8.hpp $(DESTDIR)$(PREFIX)/include/lug
	@chmod 644 $(DESTDIR)$(PREFIX)/include/lug/utf8.hpp

uninstall:
	@echo removing header files from $(DESTDIR)$(PREFIX)/include/lug
	@rm -f $(DESTDIR)$(PREFIX)/include/lug/lug.hpp
	@rm -f $(DESTDIR)$(PREFIX)/include/lug/unicode.hpp
	@rm -f $(DESTDIR)$(PREFIX)/include/lug/utf8.hpp
	@rmdir $(DESTDIR)$(PREFIX)/include/lug

.PHONY: all options samples tests clean dist install uninstall
