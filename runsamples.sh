#!/bin/sh

# lug - Embedded DSL for PE grammar parser combinators in C++
# Copyright (c) 2017-2025 Jesse W. Towner
# See LICENSE.md file for license details

# Usage: runsamples.sh <file1>.tests [<file2>.tests ...]

for tests_file in "$@"; do
	if [ ! -f "$tests_file" ]; then
		printf "Error: Test file '%s' not found\n" "$tests_file"
		continue
	fi

	tests_content=$(grep -v '^#' "$tests_file" | grep -v '^[[:space:]]*$' | tr '\r\n' ' ')
	tests_dir=$(dirname "$tests_file")

	while [ -n "$tests_content" ]; do
		program=$(echo "$tests_content" | awk '{print $1}')
		input_ext=$(echo "$tests_content" | awk '{print $2}')
		tests_content=$(echo "$tests_content" | cut -d' ' -f3-)
		if [ -z "$program" ] || [ -z "$input_ext" ]; then
			continue
		fi
		
		printf "testing %s <(*.%s)\n" "$tests_dir/$program" "$input_ext"
		printf "=============================================\n"

		program_name=$(basename "$program")

		for infile in "$tests_dir"/*."$input_ext"; do
			infile_base=$(basename "${infile%.*}")
			outfile="$tests_dir/$program_name.$infile_base.out"

			if [ ! -f "$outfile" ]; then
				printf "[SKIP] %s (no output file)\n" "$infile"
				continue
			fi

			output=$("$tests_dir/$program" <"$infile" 2>&1)
			ret=$?

			if [ $ret -ne 0 ]; then
				printf "[FAIL] %s (return code %d)\n" "$infile" "$ret"
				printf "%s\n" "$output"
				continue
			fi

			if ! printf "%s\n" "$output" | diff - "$outfile" >/dev/null 2>&1; then
				printf "[FAIL] %s (output mismatch)\n" "$infile"
				printf "Diff between expected and actual:\n"
				printf "%s\n" "$output" | diff -y "$outfile" - | cat -n | grep -v -e '($'
				continue
			fi

			printf "[PASS] %s\n" "$infile"
		done
		
		printf "=============================================\n"
	done
done

printf "all done\n"