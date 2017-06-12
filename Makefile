# lug - Embedded DSL for PE grammar parsers in C++
# See LICENSE file for copyright and license details

# distribution version
VERSION = 1.0a

# file paths
PREFIX = /usr/local

# toolchain
CXX = clang++
CXXSTD = -std=c++17 -stdlib=libc++
CXXFLAGS = $(CXXSTD) -pedantic -Wall -Os -I.
LDFLAGS = $(CXXSTD) -s

SAMPLES = calc test
SAMPLES_BIN = $(SAMPLES:%=samples/%)
SAMPLES_OBJ = $(SAMPLES:%=samples/%.o)

all: options samples

.cpp.o:
	@echo CXX $<
	@$(CXX) -c $(CXXFLAGS) -o $@ $<

$(SAMPLES_OBJ): lug.hpp

$(SAMPLES_BIN): $(SAMPLES_OBJ)
	@echo CXX -o $@
	@$(CXX) -o $@ $@.o $(LDFLAGS)

samples: $(SAMPLES_BIN)

options:
	@echo lug build options:
	@echo "CXX       = $(CXX)"
	@echo "CXXSTD    = $(CXXSTD)"
	@echo "CXXLAGS   = $(CXXFLAGS)"
	@echo "LDFLAGS   = $(LDFLAGS)"

clean:
	@echo cleaning
	@rm -f $(SAMPLES_BIN) $(SAMPLES_OBJ) lug-$(VERSION).tar.gz

dist: clean
	@echo creating dist tarball
	@mkdir -p lug-$(VERSION)
	@cp -R README.md LICENSE.md Makefile lug.hpp lug.sln msvs/ $(SAMPLES_SRC) lug-$(VERSION)
	@tar -cf lug-$(VERSION).tar lug-$(VERSION)
	@gzip lug-$(VERSION).tar
	@rm -rf lug-$(VERSION)

install: all
	@echo installing header files to $(DESTDIR)$(PREFIX)/lug
	@mkdir -p $(DESTDIR)$(PREFIX)/include/lug
	@cp -f lug.hpp $(DESTDIR)$(PREFIX)/include/lug
	@chmod 644 $(DESTDIR)$(PREFIX)/include/lug/lug.hpp

uninstall:
	@echo removing header files from $(DESTDIR)$(PREFIX)/lug
	@rm -f $(DESTDIR)$(PREFIX)/include/lug/lug.hpp
	@rm -f $(DESTDIR)$(PREFIX)/include/lug

.PHONY: all options samples clean dist install uninstall
