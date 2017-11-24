#!/bin/sh

# lug - Embedded DSL for PE grammar parsers in C++
# See LICENSE file for copyright and license details

printf "running %s\n" "$1"
printf "=============================================\n"

shift
while [ $# -gt 0 ]; do
	printf "%s\n" "$1" && $1;
	shift
done

printf "=============================================\n"
printf "all done\n"