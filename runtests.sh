#!/bin/sh

# lug - Embedded DSL for PE grammar parser combinators in C++
# Copyright (c) 2017-2025 Jesse W. Towner
# See LICENSE.md file for license details

# Usage: runtests.sh <program1> [<program2> ...]

usage() {
	printf "Usage: runtests.sh <program1> [<program2> ...]\n\n"
	printf "Runs the specified test programs and reports their results.\n"
	printf "Each program is executed and its output is captured.\n"
	printf "Programs that exit with code 0 are marked as PASS.\n"
	printf "Programs that exit with non-zero codes are marked as FAIL.\n\n"
	printf "Example:\n"
	printf "  ./runtests.sh tests/terminals tests/nonterminals\n"
}

if [ $# -eq 0 ]; then
	usage
	exit 1
fi

run_status=0

printf "running tests\n"
printf "=============================================\n"

while [ $# -gt 0 ]; do
	if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
		usage
		exit 1
	fi
	output=$($1 2>&1)
	ret=$?
	if [ $ret -eq 0 ]; then
		printf "[PASS] "
	else
		printf "[FAIL] "
		run_status=1
	fi
	printf "%s\n" "$1"
	if [ -n "$output" ]; then
		printf "%s\n" "$output"
	fi
	shift
done

printf "=============================================\n"
printf "all done\n"

exit $run_status