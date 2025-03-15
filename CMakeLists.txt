# lug - Embedded DSL for PE grammar parser combinators in C++
# Copyright (c) 2017-2025 Jesse W. Towner
# See LICENSE.md file for license details

cmake_minimum_required(VERSION 3.13)

project(
  lug VERSION 0.5.0
  DESCRIPTION "Embedded DSL for PE grammar parser combinators in C++"
  HOMEPAGE_URL "https://github.com/jwtowner/lug"
  LANGUAGES CXX)

option(BUILD_SAMPLES "Build sample applications" ON)
option(BUILD_TESTS "Build tests" ON)
option(BUILD_TOOLS "Build tools" OFF)
option(CI_BUILD "Build is running on CI" OFF)

include(CMakePackageConfigHelpers)
include(CTest)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

add_library(lug INTERFACE)

target_include_directories(lug INTERFACE
  $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include>)

target_compile_features(lug INTERFACE cxx_std_17)

write_basic_package_version_file(
  lug-config-version.cmake
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY SameMajorVersion)

install(
  TARGETS lug
  EXPORT lug-targets
  INCLUDES DESTINATION include)

install(
  EXPORT lug-targets
  FILE lug-config-version.cmake
  NAMESPACE lug::
  DESTINATION lib/cmake/lug)

install(DIRECTORY include/lug/ DESTINATION include/lug)

install(
  FILES "${CMAKE_CURRENT_BINARY_DIR}/lug-config-version.cmake"
  DESTINATION lib/cmake/lug)

# Sets up the toolchain for an executable target
function(setup_executable_target target_name)
  set_target_properties(${target_name} PROPERTIES LANGUAGE CXX)
  target_compile_features(${target_name} PRIVATE cxx_std_17)
  target_link_libraries(${target_name} PRIVATE lug)
  target_include_directories(${target_name} PRIVATE ${PROJECT_SOURCE_DIR}/include)
  if (CMAKE_CXX_COMPILER_ID MATCHES "Clang" OR CMAKE_CXX_COMPILER_ID MATCHES "GNU")
    target_compile_options(${target_name} PRIVATE
      -Wall -Wconversion -Wextra -Wextra-semi -Wshadow -Wsign-conversion -Wsuggest-override
      -Wunreachable-code -Wno-parentheses -Wno-logical-not-parentheses
      $<$<BOOL:${CI_BUILD}>:-Werror>
      $<$<OR:$<CONFIG:Release>,$<CONFIG:MinSizeRel>>:-ffunction-sections -fdata-sections>)
    if (UNIX AND NOT APPLE)
      target_link_options(${target_name} PRIVATE
        $<$<OR:$<CONFIG:Release>,$<CONFIG:MinSizeRel>>:-s>)
    endif()
  endif()
  if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    target_compile_options(${target_name} PRIVATE -Wextra-semi-stmt -Wshadow-all)
  endif()
  if (MSVC)
    target_compile_options(${target_name} PRIVATE
      /bigobj /W4 /permissive- /sdl /utf-8 /EHsc /GR /Za /Zc:__cplusplus /Zc:inline
      /Zc:referenceBinding /Zc:rvalueCast /Zc:strictStrings /Zc:throwingNew /Zc:wchar_t
      $<$<BOOL:${CI_BUILD}>:/WX>)
    target_link_options(${target_name} PRIVATE
      $<$<OR:$<CONFIG:Release>,$<CONFIG:MinSizeRel>>:/OPT:REF /OPT:ICF>)
  endif()
endfunction()

# Adds a test to the build that runs a command and compares the output to an
# expected output file. The test will fail if the command does not return the
# expected status or if the output does not match the expected output file.
function(add_sample_testplan_test)
  cmake_parse_arguments(PARSE_ARGV 0 ARG "" 
    "TEST_NAME;TEST_DIR;COMMAND_EXECUTABLE;EXPECTED_STATUS;EXPECTED_OUTPUT" "COMMAND_ARGS")
  set(input_file "")
  if(ARG_COMMAND_ARGS MATCHES ".*<[ \t]*([^ \t><]+).*")
    set(input_file "${CMAKE_MATCH_1}")
    string(REGEX REPLACE "<[ \t]*[^ \t><]+" "" ARG_COMMAND_ARGS "${ARG_COMMAND_ARGS}")
    string(STRIP "${ARG_COMMAND_ARGS}" ARG_COMMAND_ARGS)
  endif()
  add_test(
    NAME ${ARG_TEST_NAME}
    COMMAND ${CMAKE_COMMAND}
      -DTEST_NAME=${ARG_TEST_NAME}
      -DTEST_EXECUTABLE=${ARG_COMMAND_EXECUTABLE}
      -DTEST_ARGS=${ARG_COMMAND_ARGS}
      -DTEST_DIR=${ARG_TEST_DIR}
      $<$<BOOL:${input_file}>:-DTEST_INPUT=${ARG_TEST_DIR}/${input_file}>
      -DEXPECTED_STATUS=${ARG_EXPECTED_STATUS}
      -DEXPECTED_OUTPUT=${ARG_TEST_DIR}/${ARG_EXPECTED_OUTPUT}
      -P "${CMAKE_BINARY_DIR}/sample_testplan_runner.cmake")
endfunction()

# Processes a sample testplan file. This function parses and executes test
# commands from a testplan file. See runsamples.sh for the original shell
# script implementation that this function mimics.
# Format: [group] followed by "command <status> outfile"
#   - command can include %pattern% for input file substitution
#   - outfile can use @ for group name and % for input filename (without extension)
function(process_sample_testplan testplan)
  get_filename_component(testplan_dir "${testplan}" DIRECTORY)
  get_filename_component(testplan_full_dir "${testplan_dir}" ABSOLUTE)
  string(REPLACE "/" "_" test_prefix "${testplan_dir}")
  set(current_group "")
  set(current_group_prefix "")
  set(group_counter 0)

  # Read the testplan file into a list of lines
  file(STRINGS "${testplan}" testplan_lines ENCODING UTF-8)
  if(NOT testplan_lines)
    message(WARNING "Testplan file '${testplan}' is empty or could not be read")
    return()
  endif()

  foreach(line ${testplan_lines})
    # Skip empty lines and comments
    if(line MATCHES "^[ \t]*#" OR line MATCHES "^[ \t]*$")
      continue()
    endif()

    # Extract group name
    if(line MATCHES "^[ \t]*\\[(.*)\\][ \t]*$")
      set(current_group "${CMAKE_MATCH_1}")  
      string(REPLACE "." "_" current_group_prefix "${current_group}")
      set(group_counter 0)
      continue()
    endif()

    # Extract command, expected status, and output file
    if(NOT (line MATCHES "^(.+)<(-?[0-9]+)>(.+)$" AND current_group))
      message(WARNING "Skipping malformed test line: '${line}' - expected format 'command <status> outfile' with a defined group")
      continue()
    endif()
    set(command "${CMAKE_MATCH_1}")
    set(expected_status "${CMAKE_MATCH_2}")
    set(outfile "${CMAKE_MATCH_3}")
    string(STRIP "${command}" command)
    string(STRIP "${outfile}" outfile)

    # Extract executable name (first token) from command
    string(REGEX MATCH "^[^ \t]+" command_executable "${command}")
    string(LENGTH "${command_executable}" command_executable_length)
    string(SUBSTRING "${command}" ${command_executable_length} -1 command_args)
    string(STRIP "${command_args}" command_args)

    # Validate executable target exists and get the command target file
    if(NOT TARGET ${command_executable})
      message(WARNING "Skipping test for command '${command}' - no target '${command_executable}'")
      continue()
    endif()
    set(command_target_file "$<TARGET_FILE:${command_executable}>")

    # Does the command have an input pattern?
    if(command_args MATCHES ".*%([^%]+)%.*")
      # Process command with input pattern
      set(pattern "${CMAKE_MATCH_1}")
      get_filename_component(pattern_dir "${pattern}" DIRECTORY)
      string(STRIP "${pattern_dir}" pattern_dir)
      if(pattern_dir)
        set(pattern_dir "${pattern_dir}/")
      endif()
      file(GLOB input_files "${testplan_dir}/${pattern}")
      foreach(infile ${input_files})
        get_filename_component(infile_base "${infile}" NAME)
        get_filename_component(infile_base_no_ext "${infile}" NAME_WE)
        string(REGEX REPLACE "%[^%]+%" "${pattern_dir}${infile_base}" resolved_command_args "${command_args}")
        string(REPLACE "@" "${current_group}" resolved_outfile "${outfile}")
        string(REPLACE "%" "${infile_base_no_ext}" resolved_outfile "${resolved_outfile}")
        math(EXPR group_counter "${group_counter} + 1")
        set(test_name "${test_prefix}_${current_group_prefix}${group_counter}_${infile_base_no_ext}")
        add_sample_testplan_test(
          TEST_NAME ${test_name}
          TEST_DIR ${testplan_full_dir}
          COMMAND_EXECUTABLE ${command_target_file}
          COMMAND_ARGS ${resolved_command_args}
          EXPECTED_STATUS ${expected_status}
          EXPECTED_OUTPUT ${resolved_outfile})
      endforeach()
    else()
      # Process command without input pattern
      string(REPLACE "@" "${current_group}" resolved_outfile "${outfile}")
      math(EXPR group_counter "${group_counter} + 1")
      set(test_name "${test_prefix}_${current_group_prefix}${group_counter}")
      add_sample_testplan_test(
        TEST_NAME ${test_name}
        TEST_DIR ${testplan_full_dir}
        COMMAND_EXECUTABLE ${command_target_file}
        COMMAND_ARGS ${command_args}
        EXPECTED_STATUS ${expected_status}
        EXPECTED_OUTPUT ${resolved_outfile})
    endif()
  endforeach()
endfunction()

# Generates a testplan runner script that can be used to run
# test commands from a testplan file.
function(generate_sample_testplan_runner)
  if(NOT EXISTS "${CMAKE_BINARY_DIR}/sample_testplan_runner.cmake")
    file(WRITE "${CMAKE_BINARY_DIR}/sample_testplan_runner.cmake" [[
      message(STATUS "TEST NAME: ${TEST_NAME}")
      message(STATUS "COMMAND: ${TEST_EXECUTABLE} ${TEST_ARGS}")
      message(STATUS "WORKING DIRECTORY: ${TEST_DIR}")
      message(STATUS "INPUT FILE: ${TEST_INPUT}")
      message(STATUS "EXPECTED OUTPUT FILE: ${EXPECTED_OUTPUT}")
      message(STATUS "EXPECTED EXIT STATUS: ${EXPECTED_STATUS}")

      separate_arguments(TEST_ARGS_LIST NATIVE_COMMAND "${TEST_ARGS}")

      if(DEFINED TEST_INPUT)
        execute_process(
          COMMAND ${TEST_EXECUTABLE} ${TEST_ARGS_LIST}
          WORKING_DIRECTORY ${TEST_DIR}
          INPUT_FILE ${TEST_INPUT}
          OUTPUT_VARIABLE out
          ERROR_VARIABLE out
          RESULT_VARIABLE status
          ENCODING UTF-8)
      else()
        execute_process(
          COMMAND ${TEST_EXECUTABLE} ${TEST_ARGS_LIST}
          WORKING_DIRECTORY ${TEST_DIR}
          OUTPUT_VARIABLE out
          ERROR_VARIABLE out
          RESULT_VARIABLE status
          ENCODING UTF-8)
      endif()

      if(NOT status EQUAL EXPECTED_STATUS)
        message(FATAL_ERROR "[FAIL] ACTUAL EXIT STATUS: ${status} EXPECTED EXIT STATUS: ${EXPECTED_STATUS}\nOutput:\n${out}")
      endif()

      string(REPLACE "\r\n" "\n" out "${out}")
      string(STRIP "${out}" out)
      file(READ "${EXPECTED_OUTPUT}" expected)
      string(REPLACE "\r\n" "\n" expected "${expected}")
      string(STRIP "${expected}" expected)

      if(NOT out STREQUAL expected)
        message(FATAL_ERROR "[FAIL] OUTPUT MISMATCH\nACTUAL:\n${out}\nEXPECTED:\n${expected}")
      endif()

      message(STATUS "[PASS] EXIT STATUS: ${status}")
    ]])
  endif()
endfunction()

if(BUILD_TOOLS)
  add_executable(makeunicode tools/makeunicode.cpp)
  setup_executable_target(makeunicode)
endif()

if(BUILD_TESTS)
  enable_testing()
  file(GLOB test_sources tests/*.cpp)
  foreach(test_source ${test_sources})
    get_filename_component(test_name ${test_source} NAME_WE)
    add_executable(${test_name} ${test_source})
    setup_executable_target(${test_name})
    add_test(NAME ${test_name} COMMAND ${test_name})
  endforeach()
endif()

if(BUILD_SAMPLES)
  file(GLOB sample_dirs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} samples/*)
  foreach(sample_dir ${sample_dirs})
    file(GLOB sample_sources ${CMAKE_CURRENT_SOURCE_DIR}/${sample_dir}/*.cpp)
    foreach(sample_source ${sample_sources})
      get_filename_component(sample_name ${sample_source} NAME_WE)
      add_executable(${sample_name} ${sample_source})
      setup_executable_target(${sample_name})
    endforeach()
  endforeach()
  file(GLOB testplan_files RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} samples/*/*.testplan)
  if(testplan_files)
    generate_sample_testplan_runner()
    foreach(testplan_file ${testplan_files})
      process_sample_testplan(${testplan_file})
    endforeach()
  endif()
endif()