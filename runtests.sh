#!/bin/sh

# lug - Embedded DSL for PE grammar parser combinators in C++
# Copyright (c) 2017-2025 Jesse W. Towner
# See LICENSE.md file for license details

# Usage: runtests.sh <program1> [<program2> ...]

printf "running tests\n"
printf "=============================================\n"

while [ $# -gt 0 ]; do
	output=$($1 2>&1)
	ret=$?
	if [ $ret -eq 0 ]; then
		printf "[PASS] "
	else
		printf "[FAIL] "
	fi
	printf "%s\n" "$1"
	if [ -n "$output" ]; then
		printf "%s\n" "$output"
	fi
	shift
done

printf "=============================================\n"
printf "all done\n"