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

all: options samples tests

.cpp.o:
	@echo CXX $<
	@$(CXX) -c $(CXXFLAGS) -o $@ $<

$(SAMPLES_OBJ): lug.hpp

$(SAMPLES_BIN): $(SAMPLES_OBJ)
	@echo CXX -o $@
	@$(CXX) -o $@ $@.o $(LDFLAGS)

samples: $(SAMPLES_BIN)

$(TESTS_OBJ): lug.hpp

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
	@cp -R README.md LICENSE.md Makefile lug.hpp lug.sln msvs/ samples/ tests/ lug-$(VERSION)
	@tar -cf lug-$(VERSION).tar lug-$(VERSION)
	@gzip lug-$(VERSION).tar
	@rm -rf lug-$(VERSION)

install: all
	@echo installing header file to $(DESTDIR)$(PREFIX)/include
	@mkdir -p $(DESTDIR)$(PREFIX)/include
	@cp -f lug.hpp $(DESTDIR)$(PREFIX)/include
	@chmod 644 $(DESTDIR)$(PREFIX)/include/lug.hpp

uninstall:
	@echo removing header file from $(DESTDIR)$(PREFIX)/include
	@rm -f $(DESTDIR)$(PREFIX)/include/lug.hpp

.PHONY: all options samples tests clean dist install uninstall
