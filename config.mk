# customize below to fit your system

# distribution version
VERSION=1.0-prerelease

# file paths
PREFIX = /usr/local
X11INC = /usr/X11R6/include
X11LIB = /usr/X11R6/lib

# includes and libs
INCS = -I.
LIBS =

# toolchain flags
CPPFLAGS =
CXXFLAGS = -std=c++1z -stdlib=libc++ -pedantic -Wall -Os ${INCS} ${CPPFLAGS}
LDFLAGS = -s ${LIBS}

# compiler and linker
CXX = clang++
