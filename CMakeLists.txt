# lug - Embedded DSL for PE grammar parser combinators in C++
# Copyright (c) 2017-2024 Jesse W. Towner
# See LICENSE.md file for license details

cmake_minimum_required(VERSION 3.10)

project(
  lug VERSION 0.3.0
  DESCRIPTION "Embedded DSL for PE grammar parser combinators in C++"
  HOMEPAGE_URL "https://github.com/jwtowner/lug"
  LANGUAGES CXX)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -fuse-ld=lld")
  set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -fuse-ld=lld")
endif()

add_library(lug INTERFACE)

target_include_directories(lug INTERFACE
  $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/lug>
  $<INSTALL_INTERFACE:include>)

target_compile_features(lug INTERFACE cxx_std_17)

include(CMakePackageConfigHelpers)

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

install(DIRECTORY lug/ DESTINATION include/lug)

install(
  FILES "${CMAKE_CURRENT_BINARY_DIR}/lug-config-version.cmake"
  DESTINATION lib/cmake/lug)

function(setup_executable_target target_name)
  set_target_properties(${target_name} PROPERTIES LANGUAGE CXX)
  target_compile_features(${target_name} PRIVATE cxx_std_17)
  target_link_libraries(${target_name} PRIVATE lug)
  target_include_directories(${target_name} PRIVATE ${PROJECT_SOURCE_DIR})
  if (CMAKE_CXX_COMPILER_ID MATCHES "Clang" OR CMAKE_CXX_COMPILER_ID MATCHES "GNU")
    target_compile_options(${target_name} PRIVATE
      -Wall -Wconversion -Wextra -Wextra-semi -Wshadow -Wsign-conversion -Wsuggest-override
      -Wunreachable-code -Wno-parentheses -Wno-logical-not-parentheses
      $<$<OR:$<CONFIG:Release>,$<CONFIG:MinSizeRel>>:-ffunction-sections -fdata-sections>)
    target_link_options(${target_name} PRIVATE
      $<$<OR:$<CONFIG:Release>,$<CONFIG:MinSizeRel>>:-s>)
  endif()
  if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    target_compile_options(${target_name} PRIVATE -stdlib=libc++ -Wextra-semi-stmt -Wshadow-all)
    target_link_options(${target_name} PRIVATE -stdlib=libc++ -fuse-ld=lld)
    if (UNIX AND NOT APPLE)
      target_link_libraries(${target_name} PRIVATE c++ c++abi m)
    endif()    
  endif()
  if (MSVC)
    target_compile_options(${target_name} PRIVATE
      /W4 /permissive /sdl /utf-8 /Za /Zc:__cplusplus /Zc:rvalueCast /Zc:inline
      /Zc:strictStrings /Zc:throwingNew /Zc:referenceBinding /Zc:wchar_t)
    target_link_options(${target_name} PRIVATE
      $<$<OR:$<CONFIG:Release>,$<CONFIG:MinSizeRel>>:/OPT:REF /OPT:ICF>)
  endif()  
endfunction()

option(BUILD_TESTS "Build tests" ON)
if(BUILD_TESTS)
  enable_testing()
  file(GLOB TEST_SOURCES tests/*.cpp)
  foreach(test_source ${TEST_SOURCES})
    get_filename_component(test_name ${test_source} NAME_WE)
    add_executable(${test_name} ${test_source})
    setup_executable_target(${test_name})
    add_test(NAME ${test_name} COMMAND ${test_name} )
  endforeach()
endif()

option(BUILD_EXAMPLES "Build sample applications" ON)
if(BUILD_EXAMPLES)
  set(SAMPLES basic calc json xml)
  foreach(sample_name ${SAMPLES})
    add_executable(${sample_name} samples/${sample_name}/${sample_name}.cpp)
    setup_executable_target(${sample_name})
  endforeach()
endif()