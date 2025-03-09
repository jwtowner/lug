#!/bin/sh

# lug - Embedded DSL for PE grammar parser combinators in C++
# Copyright (c) 2017-2025 Jesse W. Towner
# See LICENSE.md file for license details

# Usage: runsamples.sh <testplan1> [<testplan2> ...]

# Print usage if no arguments are provided
usage() {
	printf "Usage: runsamples.sh <testplan1> [<testplan2> ...]\n\n"
	printf "Runs the given sample testplan files. Each testplan file\n"
	printf "should contain test groups and commands in the format:\n\n"
	printf "  [GROUP-NAME]\n"
	printf "  COMMAND <S> OUTPUT-FILE\n\n"
	printf "Where:\n"
	printf "  * [GROUP-NAME]    Defines a named group of related tests\n"
	printf "  * COMMAND         The shell command to execute, may contain an input file pattern\n"
	printf "  * <S>             Expected exit status code of the command, 0 for success\n"
	printf "  * OUTPUT-FILE     Path of file containing expected program output\n\n"
	printf "Special Patterns:\n"
	printf "  * %%PATTERN%%       Input file pattern (e.g. %%*.json%%) in shell command\n"
	printf "  * @               Replaced by current group name in expected output paths\n"
	printf "  * %%               Replaced by input file (without extension) in expected output paths\n\n"
	printf "Rules:\n"
	printf "  * Lines starting with # are comments\n"
	printf "  * Empty lines are ignored\n"
	printf "  * Commands must be in a named group\n"
	printf "  * Commands are run in order from the testplan directory\n"
	printf "  * Expected output files are relative to the testplan directory\n\n"
	printf "Example Testplan:\n"
	printf "  # samples/demo/.testplan\n"
	printf "  [demo-tests]\n"
	printf "  demo --verbose %%*.txt%% <0> @.%%.out\n"
	printf "  demo --format --verbose %%*.txt%% <0> @.%%.formatted\n\n"
	printf "  $ ./runsamples.sh samples/demo/.testplan\n\n"
	printf "  This will:\n"
	printf "    1. Find all .txt files in the testplan directory\n"
	printf "    2. Run each command replacing the input pattern with matching files\n"
	printf "    3. Compare exit status codes of commands with expected value\n"
	printf "    4. Compare output against files matching \"demo-tests.*.(out|formatted)\"\n"
	printf "    5. Print results to the console\n"
}

# Trim leading and trailing whitespace from a string
trim() {
	echo "$1" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//'
}

# Parse the specified component from a test command line
parse_test_command() {
	echo "$1" | sed -E "s/^(.+)<(-?[[:digit:]]+)>(.+)$/\\$2/"
}

# Run a test command and compare the output to the expected output
run_test_command() {
	test_dir="$1"
	test_command="$2"
	test_expected_status="$3"
	test_expected_outfile="$4"

	# Skip test if expected output file does not exist
	if [ ! -f "$test_dir/$test_expected_outfile" ]; then
		printf "[SKIP] %s (NO OUTPUT FILE: \"%s\")\n" "$test_command" "$test_dir/$test_expected_outfile"
		return 1
	fi

	# Run test command and capture output
	test_actual_output=$(cd "$test_dir" && eval "./$test_command" 2>&1)
	test_status=$?
	test_pass=0

	# Check if command failed
	test_expected_status=$((test_expected_status + 0))
	if [ $test_status -ne $test_expected_status ]; then
		printf "[FAIL] %s (EXIT STATUS: %d) (EXPECTED: %d)\n" "$test_command" $test_status $test_expected_status
		test_pass=1
	fi

	# Fail test if output does not match expected output
	if ! printf "%s\n" "$test_actual_output" | diff - "$test_dir/$test_expected_outfile" >/dev/null 2>&1; then
		printf "[FAIL] %s (OUTPUT MISMATCH)\n" "$test_command"
		printf "Diff between expected and actual:\n"
		printf "%s\n" "$test_actual_output" | diff -y "$test_dir/$test_expected_outfile" - | cat -n | grep -v -e '($'
		test_pass=1
	fi

	# Return 1 if test failed
	if [ $test_pass -ne 0 ]; then
		return 1
	fi

	# Print PASS if test passed
	printf "[PASS] %s\n" "$test_command"
	return 0
}

if [ $# -eq 0 ]; then
	usage
	exit 1
fi

run_status=0

printf "running samples\n"
printf "=============================================\n"

# Run each testplan file
for testplan_file in "$@"; do
	# Print usage if help flag is provided
	if [ "$testplan_file" = "-h" ] || [ "$testplan_file" = "--help" ]; then
		usage
		exit 1
	fi

	# Skip if file does not exist
	if [ ! -f "$testplan_file" ]; then
		printf "Error: Test file '%s' not found\n" "$testplan_file"
		continue
	fi

	# Get directory of testplan file
	testplan_dir=$(dirname "$testplan_file")
	if [ -z "$testplan_dir" ] || [ "$testplan_dir" = "." ]; then
		testplan_dir="."
	fi

	# Current group of tests
	current_group=""

	# Read file line by line, skipping comments and empty lines
	while IFS= read -r line <&3 || [ -n "$line" ]; do
		# Skip comments and blank or empty lines
		[ -z "$line" ] || echo "$line" | grep -q "^[[:space:]]*#\|^[[:space:]]*$" && continue

		# Check if this is a group header
		group=$(echo "$line" | sed 's/^[[:space:]]*\[\(.*\)\][[:space:]]*$/\1/')
		if [ "$group" != "$line" ]; then
			current_group=$(trim "$group")
			printf "\n[%s]\n" "$current_group"
			printf "=============================================\n"
			continue
		fi

		# Split line into command, status, and outfile
		command=$(parse_test_command "$line" 1)
		status=$(parse_test_command "$line" 2)
		outfile=$(parse_test_command "$line" 3)
		if [ "$command" = "$line" ] || [ "$status" = "$line" ] || [ "$outfile" = "$line" ]; then
			printf "Error: Invalid test command: %s\n" "$line"
			run_status=1
			continue;
		fi

		# Strip leading and trailing whitespace from command and outfile
		command=$(trim "$command")
		outfile=$(trim "$outfile")

		# Skip test if no group is defined yet
		if [ -z "$current_group" ]; then
			printf "Error: No group defined for test command: %s <%s> %s\n" "$command" "$status" "$outfile"
			run_status=1
			continue
		fi

		# Extract input pattern if it exists
		pattern=$(echo "$command" | sed -E 's/.*%([^%]+)%.*/\1/')
		if [ "$pattern" = "$command" ]; then
			# Handle commands without input pattern
			resolved_outfile=$(echo "$outfile" | sed "s/@/${current_group}/g")
			run_test_command "$testplan_dir" "$command" "$status" "$resolved_outfile" || run_status=1
		else
			# Process each matching input file
			for infile in "$testplan_dir"/$pattern; do
				[ -f "$infile" ] || continue

				# Get base name and base name without extension
				infile_base=$(basename "$infile")
				infile_base_no_ext=$(echo "$infile_base" | sed 's/\.[^.]*$//')

				# Replace the input pattern in the command with the actual input file path
				resolved_command=$(echo "$command" | sed -E "s/%[^%]+%/${infile_base}/g")

				# Replace special tokens in output file path
				resolved_outfile=$(echo "$outfile" | sed -e "s/@/${current_group}/g" -e "s/%/${infile_base_no_ext}/g")

				# Execute command with input file
				run_test_command "$testplan_dir" "$resolved_command" "$status" "$resolved_outfile" || run_status=1
			done
		fi
	done 3< "$testplan_file"
done

printf "\n=============================================\n"
printf "all done\n"

exit $run_status