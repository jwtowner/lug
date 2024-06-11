# lug - Embedded DSL for PE grammar parser combinators in C++
# Copyright (c) 2017-2024 Jesse W. Towner
# See LICENSE.md file for license details

param (
    [Parameter(Mandatory=$true)]
    [string]$UCD_VERSION
)

$UCD_URL = "https://www.unicode.org/Public/${UCD_VERSION}/ucd/"

$UCD_FILES = @(
    "Blocks.txt",
    "CaseFolding.txt",
    "DerivedAge.txt",
    "DerivedCoreProperties.txt",
    "EastAsianWidth.txt",
    "PropList.txt",
    "ScriptExtensions.txt",
    "Scripts.txt",
    "extracted/DerivedGeneralCategory.txt"
)

foreach ($FILE in $UCD_FILES) {
    Write-Host "downloading ${UCD_URL}${FILE}"
    $OUT_PATH = "ucd/${FILE}"
    $OUT_DIR = Split-Path -Path $OUT_PATH -Parent
    if (!(Test-Path -Path $OUT_DIR)) {
        New-Item -ItemType Directory -Path $OUT_DIR | Out-Null
    }    
    Invoke-WebRequest -Uri "${UCD_URL}${FILE}" -OutFile "${OUT_PATH}" -SkipHttpErrorCheck -ErrorAction Stop
}