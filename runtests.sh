#!/bin/sh

# lug - Embedded DSL for PE grammar parser combinators in C++
# Copyright (c) 2017-2024 Jesse W. Towner
# See LICENSE.md file for license details

printf "running %s\n" "$1"
printf "=============================================\n"

shift
while [ $# -gt 0 ]; do
	printf "%s\n" "$1" && $1;
	shift
done

printf "=============================================\n"
printf "all done\n"