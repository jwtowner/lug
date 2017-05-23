# lug - Embedded DSL for PE grammar parsers in C++
# See LICENSE file for copyright and license details

include config.mk

SRC = samples/calc.cpp
OBJ = ${SRC:.cpp=.o}

all: options calc

.cpp.o:
	@echo CXX $<
	@${CXX} -c ${CXXFLAGS} $<

${OBJ}: config.mk

calc: ${OBJ}
	@echo CXX -o $@
	@${CXX} -o $@ ${OBJ} ${LDFLAGS}

options:
	@echo lug build options:
	@echo "CXXLAGS   = ${CXXFLAGS}"
	@echo "LDFLAGS   = ${LDFLAGS}"
	@echo "CXX       = ${CXX}"

clean:
	@echo cleaning
	@rm -f calc ${OBJ} lug-${VERSION}.tar.gz

dist: clean
	@echo creating dist tarball
	@mkdir -p lug-${VERSION}
	@cp -R LICENSE Makefile README config.mk ${SRC} lug-${VERSION}
	@tar -cf lug-${VERSION}.tar lug-${VERSION}
	@gzip lug-${VERSION}.tar
	@rm -rf lug-${VERSION}

install: all
	@echo installing header files to ${DESTDIR}${PREFIX}/lug
	@mkdir -p ${DESTDIR}${PREFIX}/include/lug
	@cp -f lug.hpp ${DESTDIR}${PREFIX}/include/lug
	@chmod 644 ${DESTDIR}${PREFIX}/include/lug/lug.hpp

uninstall:
	@echo removing header files from ${DESTDIR}${PREFIX}/lug
	@rm -f ${DESTDIR}${PREFIX}/include/lug/lug.hpp
	@rm -f ${DESTDIR}${PREFIX}/include/lug

.PHONY: all options clean dist install uninstall
