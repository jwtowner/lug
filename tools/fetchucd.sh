#!/bin/sh

# lug - Embedded DSL for PE grammar parser combinators in C++
# Copyright (c) 2017 Jesse W. Towner
# See LICENSE.md file for license details

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <UCD_VERSION>"
    exit 1
fi

UCD_VERSION=$1
UCD_URL="https://www.unicode.org/Public/${UCD_VERSION}/ucd/"

UCD_FILES="
Blocks.txt
CaseFolding.txt
DerivedAge.txt
DerivedCoreProperties.txt
EastAsianWidth.txt
PropList.txt
ScriptExtensions.txt
Scripts.txt
extracted/DerivedGeneralCategory.txt
"

for FILE in $UCD_FILES; do
    echo "downloading ${UCD_URL}${FILE}"
    mkdir -p "ucd/$(dirname ${FILE})"
    curl -s -S -o "ucd/${FILE}" "${UCD_URL}${FILE}"
done