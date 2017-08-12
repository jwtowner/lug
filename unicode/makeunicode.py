#! /usr/bin/python3

##############################################################################
# Multistage Unicode Table Builder
# (c) Peter Kankowski 2008
# (c) Philip Hazel 2008-2017
# (c) Zoltan Herczeg 2010-2017
#
# This script was originally submitted to the PCRE project by Peter Kankowski
# as part of the upgrading of Unicode property support and is distributed
# under the terms of the PCRE2 license, see the accompanying LICENSE file for
# details.
#
# $ python3 ./make-unicode.py >../lug/unicode.hpp
#
# It requires the following four Unicode Character Database tables to be in
# the same directory as the script: DerivedGeneralCategory.txt,
# GraphemeBreakProperty.txt, Scripts.txt, and CaseFolding.txt.
#
# The latest versions of these files can be located at the Unicode Consortium's
# UNIDATA FTP directory at ftp://ftp.unicode.org/Public/UNIDATA
#
# 01-March-2010:     Updated list of scripts for Unicode 5.2.0
# 30-April-2011:     Updated list of scripts for Unicode 6.0.0
#    July-2012:      Updated list of scripts for Unicode 6.1.0
# 20-August-2012:    Added scan of GraphemeBreakProperty.txt and added a new 
#                      field in the record to hold the value. Luckily, the 
#                      structure had a hole in it, so the resulting table is
#                      not much bigger than before.
# 18-September-2012: Added code for multiple caseless sets. This uses the
#                      final hole in the structure.
# 30-September-2012: Added RegionalIndicator break property from Unicode 6.2.0
# 13-May-2014:       Updated for PCRE2
# 03-June-2014:      Updated for Python 3
# 20-June-2014:      Updated for Unicode 7.0.0
# 12-August-2014:    Updated to put Unicode version into the file
# 19-June-2015:      Updated for Unicode 8.0.0
# 02-July-2017:      Updated for Unicode 10.0.0
# 28-July-2017:      Adapted for use in lug
##############################################################################

import math
import re
import string
import sys

MAX_UNICODE = 0x110000
REPLACEMENT = 0xFFFD

# Parse a line of Scripts.txt, GraphemeBreakProperty.txt or DerivedGeneralCategory.txt
def make_get_names(enum):
        return lambda chardata: enum.index(chardata[1])

# Parse a line of CaseFolding.txt
def get_other_case(chardata):
        if chardata[1] == 'C' or chardata[1] == 'S':
          return int(chardata[2], 16) - int(chardata[0], 16)
        return 0

# Read the whole table in memory, setting/checking the Unicode version
def read_table(file_name, get_value, default_value):
        file = open(file_name, 'r', encoding='utf-8')
        table = [default_value] * MAX_UNICODE
        for line in file:
                line = re.sub(r'#.*', '', line)
                chardata = list(map(str.strip, line.split(';')))
                if len(chardata) <= 1:
                        continue
                value = get_value(chardata)
                m = re.match(r'([0-9a-fA-F]+)(\.\.([0-9a-fA-F]+))?$', chardata[0])
                char = int(m.group(1), 16)
                if m.group(3) is None:
                        last = char
                else:
                        last = int(m.group(3), 16)            
                for i in range(char, last + 1):
                        # It is important not to overwrite a previously set
                        # value because in the CaseFolding file there are lines
                        # to be ignored (returning the default value of 0) 
                        # which often come after a line which has already set 
                        # data.   
                        if table[i] == default_value: 
                          table[i] = value
        file.close()
        return table

# Get the smallest possible C language type for the values
def get_type_size(table):
        type_size = [("std::uint_least8_t", 1), ("std::uint_least16_t", 2), ("std::uint_least32_t", 4),
                     ("std::int_least8_t", 1), ("std::int_least16_t", 2), ("std::int_least32_t", 4)]
        limits = [(0, 255), (0, 65535), (0, 4294967295),
                  (-128, 127), (-32768, 32767), (-2147483648, 2147483647)]
        minval = min(table)
        maxval = max(table)
        for num, (minlimit, maxlimit) in enumerate(limits):
                if minlimit <= minval and maxval <= maxlimit:
                        return type_size[num]
        else:
                raise OverflowError("Too large to fit into C++ types")

def get_tables_size(*tables):
        total_size = 0
        for table in tables:
                type, size = get_type_size(table)
                total_size += size * len(table)
        return total_size

# Compress the table into the two stages
def compress_table(table, block_size):
        blocks = {} # Dictionary for finding identical blocks
        stage1 = [] # Stage 1 table contains block numbers (indices into stage 2 table)
        stage2 = [] # Stage 2 table contains the blocks with property values
        table = tuple(table)
        for i in range(0, len(table), block_size):
                block = table[i:i+block_size]
                start = blocks.get(block)
                if start is None:
                        # Allocate a new block
                        start = len(stage2) / block_size
                        stage2 += block
                        blocks[block] = start
                stage1.append(start)
        return stage1, stage2

# Print a table
def print_table(table, table_name, block_size = None):
        type, size = get_type_size(table)
        N_PER_LINE = 32
        print("\tstatic constexpr %s %s[] = {" % (type, table_name))
        table = tuple(table)
        if block_size is None:
                fmt = "%3d," * N_PER_LINE
                mult = MAX_UNICODE / len(table)
                for i in range(0, len(table), N_PER_LINE):
                        print("\t" + (fmt % (table[i:i+N_PER_LINE])))
        else:
                rem = len(table) % N_PER_LINE
                end = len(table) - rem
                fmt = "%3d," * N_PER_LINE 
                for i in range(0, end, N_PER_LINE):
                        print("\t" + (fmt % table[i:i+N_PER_LINE]))
                if block_size < N_PER_LINE and end < len(table):
                        print("\t" + (("%3d," * block_size) % table[end:len(table)]))
        print("\t};\n")

# Extract the unique combinations of properties into records
def combine_tables(*tables):
        records = {}
        index = []
        for t in zip(*tables):
                i = records.get(t)
                if i is None:
                        i = records[t] = len(records)
                index.append(i)
        return index, records

def get_record_size(records):
        size = 0
        for i in range(len(records[0])):
                record_slice = [record[i] for record in records]
                slice_type, slice_size = get_type_size(record_slice)
                # add padding: round up to the nearest power of slice_size
                size = (size + slice_size - 1) & -slice_size
                size += slice_size
        # round up to the first item of the next structure in array
        record_slice = [record[0] for record in records]
        slice_type, slice_size = get_type_size(record_slice)
        size = (size + slice_size - 1) & -slice_size
        return size

def test_record_size():
        tests = [ \
          ( [(3,), (6,), (6,), (1,)], 1 ), \
          ( [(300,), (600,), (600,), (100,)], 2 ), \
          ( [(25, 3), (6, 6), (34, 6), (68, 1)], 2 ), \
          ( [(300, 3), (6, 6), (340, 6), (690, 1)], 4 ), \
          ( [(3, 300), (6, 6), (6, 340), (1, 690)], 4 ), \
          ( [(300, 300), (6, 6), (6, 340), (1, 690)], 4 ), \
          ( [(3, 100000), (6, 6), (6, 123456), (1, 690)], 8 ), \
          ( [(100000, 300), (6, 6), (123456, 6), (1, 690)], 8 ), \
        ]
        for test in tests:
            size = get_record_size(test[0])
            assert(size == test[1])

def print_records(records, record_size):
        records = list(zip(list(records.keys()), list(records.values())))
        records.sort(key = lambda x: x[1])
        print('\tstatic constexpr rune_record records[] = {')
        for i, record in enumerate(records):
                print('\t' + (('{' + '%6d, ' * len(record[0]) + '}') % (record[0])))
        print('\t};\n')

script_names = ['Arabic', 'Armenian', 'Bengali', 'Bopomofo', 'Braille', 'Buginese', 'Buhid', 'Canadian_Aboriginal', \
 'Cherokee', 'Common', 'Coptic', 'Cypriot', 'Cyrillic', 'Deseret', 'Devanagari', 'Ethiopic', 'Georgian', \
 'Glagolitic', 'Gothic', 'Greek', 'Gujarati', 'Gurmukhi', 'Han', 'Hangul', 'Hanunoo', 'Hebrew', 'Hiragana', \
 'Inherited', 'Kannada', 'Katakana', 'Kharoshthi', 'Khmer', 'Lao', 'Latin', 'Limbu', 'Linear_B', 'Malayalam', \
 'Mongolian', 'Myanmar', 'New_Tai_Lue', 'Ogham', 'Old_Italic', 'Old_Persian', 'Oriya', 'Osmanya', 'Runic', \
 'Shavian', 'Sinhala', 'Syloti_Nagri', 'Syriac', 'Tagalog', 'Tagbanwa', 'Tai_Le', 'Tamil', 'Telugu', 'Thaana', \
 'Thai', 'Tibetan', 'Tifinagh', 'Ugaritic', 'Yi', \
# New for Unicode 5.0
 'Balinese', 'Cuneiform', 'Nko', 'Phags_Pa', 'Phoenician', \
# New for Unicode 5.1
 'Carian', 'Cham', 'Kayah_Li', 'Lepcha', 'Lycian', 'Lydian', 'Ol_Chiki', 'Rejang', 'Saurashtra', 'Sundanese', 'Vai', \
# New for Unicode 5.2
 'Avestan', 'Bamum', 'Egyptian_Hieroglyphs', 'Imperial_Aramaic', \
 'Inscriptional_Pahlavi', 'Inscriptional_Parthian', \
 'Javanese', 'Kaithi', 'Lisu', 'Meetei_Mayek', \
 'Old_South_Arabian', 'Old_Turkic', 'Samaritan', 'Tai_Tham', 'Tai_Viet', \
# New for Unicode 6.0.0
 'Batak', 'Brahmi', 'Mandaic', \
# New for Unicode 6.1.0
 'Chakma', 'Meroitic_Cursive', 'Meroitic_Hieroglyphs', 'Miao', 'Sharada', 'Sora_Sompeng', 'Takri',
# New for Unicode 7.0.0
 'Bassa_Vah', 'Caucasian_Albanian', 'Duployan', 'Elbasan', 'Grantha', 'Khojki', 'Khudawadi',
 'Linear_A', 'Mahajani', 'Manichaean', 'Mende_Kikakui', 'Modi', 'Mro', 'Nabataean',
 'Old_North_Arabian', 'Old_Permic', 'Pahawh_Hmong', 'Palmyrene', 'Psalter_Pahlavi',
 'Pau_Cin_Hau', 'Siddham', 'Tirhuta', 'Warang_Citi',
# New for Unicode 8.0.0
 'Ahom', 'Anatolian_Hieroglyphs', 'Hatran', 'Multani', 'Old_Hungarian',
 'SignWriting',
# New for Unicode 10.0.0
 'Adlam', 'Bhaiksuki', 'Marchen', 'Newa', 'Osage', 'Tangut', 'Masaram_Gondi',
 'Nushu', 'Soyombo', 'Zanabazar_Square'
 ]
 
category_names = ['Cc', 'Cf', 'Cn', 'Co', 'Cs', 'Ll', 'Lm', 'Lo', 'Lt', 'Lu',
  'Mc', 'Me', 'Mn', 'Nd', 'Nl', 'No', 'Pc', 'Pd', 'Pe', 'Pf', 'Pi', 'Po', 'Ps',
  'Sc', 'Sk', 'Sm', 'So', 'Zl', 'Zp', 'Zs' ]

break_property_names = ['CR', 'LF', 'Control', 'Extend', 'Prepend',
  'SpacingMark', 'L', 'V', 'T', 'LV', 'LVT', 'Regional_Indicator', 'Other',
  'E_Base', 'E_Modifier', 'E_Base_GAZ', 'ZWJ', 'Glue_After_Zwj' ]

test_record_size()
unicode_version = ""

category = read_table('DerivedGeneralCategory.txt', make_get_names(category_names), category_names.index('Cn'))
other_case = read_table('CaseFolding.txt', get_other_case, 0)
#script = read_table('Scripts.txt', make_get_names(script_names), script_names.index('Common'))
#break_props = read_table('GraphemeBreakProperty.txt', make_get_names(break_property_names), break_property_names.index('Other'))

# This block of code was added by PH in September 2012. I am not a Python 
# programmer, so the style is probably dreadful, but it does the job. It scans 
# the other_case table to find sets of more than two characters that must all 
# match each other caselessly. Later in this script a table of these sets is 
# written out. However, we have to do this work here in order to compute the 
# offsets in the table that are inserted into the main table.

# The CaseFolding.txt file lists pairs, but the common logic for reading data
# sets only one value, so first we go through the table and set "return" 
# offsets for those that are not already set.

for c in range(MAX_UNICODE - 1):
  if other_case[c] != 0 and other_case[c + other_case[c]] == 0:
    other_case[c + other_case[c]] = -other_case[c] 

# Now scan again and create equivalence sets.

sets = []

for c in range(MAX_UNICODE - 1):
  o = c + other_case[c]

  # Trigger when this character's other case does not point back here. We
  # now have three characters that are case-equivalent. 
 
  if other_case[o] != -other_case[c]:
    t = o + other_case[o]
    
    # Scan the existing sets to see if any of the three characters are already 
    # part of a set. If so, unite the existing set with the new set.
 
    appended = 0 
    for s in sets:
      found = 0 
      for x in s:
        if x == c or x == o or x == t:
          found = 1
    
      # Add new characters to an existing set
       
      if found:
        found = 0 
        for y in [c, o, t]:
          for x in s:
            if x == y:
              found = 1
          if not found:
            s.append(y)
        appended = 1
        
    # If we have not added to an existing set, create a new one.

    if not appended:     
      sets.append([c, o, t])

# End of loop looking for caseless sets.

# Now scan the sets and set appropriate offsets for the characters.

caseless_offsets = [0] * MAX_UNICODE

offset = 1;
for s in sets:
  for x in s:   
    caseless_offsets[x] = offset
  offset += len(s) + 1

# End of block of code for creating offsets for caseless matching sets.

# Combine the tables

table, records = combine_tables(category, caseless_offsets, other_case)
record_size = get_record_size(list(records.keys()))

# Find the optimum block size for the two-stage table
min_size = sys.maxsize
for block_size in [2 ** i for i in range(5,10)]:
        size = len(records) * record_size
        stage1, stage2 = compress_table(table, block_size)
        size += get_tables_size(stage1, stage2)
        #print "/* block size %5d  => %5d bytes */" % (block_size, size)
        if size < min_size:
                min_size = size
                min_stage1, min_stage2 = stage1, stage2
                min_block_size = block_size

# Find the optimum block size for 3-stage table
min_size3 = sys.maxsize
for stage3_block in [2 ** i for i in range(2,6)]:
        stage_i, stage3 = compress_table(table, stage3_block)
        for stage2_block in [2 ** i for i in range(5,10)]:
                size = len(records) * 4
                stage1, stage2 = compress_table(stage_i, stage2_block)
                size += get_tables_size(stage1, stage2, stage3)
                # print "/* %5d / %3d  => %5d bytes */" % (stage2_block, stage3_block, size)
                if size < min_size3:
                        min_size3 = size
                        min_stage1, min_stage2, min_stage3 = stage1, stage2, stage3
                        min_stage2_block, min_stage3_block = stage2_block, stage3_block

print("// lug - Embedded DSL for PE grammar parser combinators in C++")
print("// Copyright (c) 2017 Jesse W. Towner")
print("// See LICENSE.md file for license details")
print()
print("// This header file is generated by the unicode/makeunicode.py script.")
print("// Do not modify this file by hand. Instead, modify and run the script to")
print("// regenerate this file.")
print()
print("#ifndef LUG_UNICODE_HPP__")
print("#define LUG_UNICODE_HPP__")
print()
print("#include <cstdint>")
print("#include <iterator>")
print("#include <numeric>")
print()
print("namespace lug::utf8")
print("{")
print()
print("inline std::size_t ucd_index(char32_t rune) noexcept {")
print_records(records, record_size)
print_table(min_stage1, 'stage1')
print_table(min_stage2, 'stage2', min_stage2_block)
print_table(min_stage3, 'stage3', min_stage3_block)
stage2_shift = int(math.ceil(math.log2(min_stage2_block)))
stage3_shift = int(math.ceil(math.log2(min_stage3_block)))
print("\tif (0x%X <= rune)" % MAX_UNICODE)
print("\t\treturn SIZE_MAX;")
print("\tstd::size_t index = stage1[rune >> {0}] << {1};".format(stage3_shift + stage2_shift, stage2_shift))
print("\tindex = stage2[index + ((rune >> {0}) & {1})] << {0};".format(stage3_shift, (1 << stage2_shift) - 1))
print("\tindex = stage3[index + (rune & {0})];".format((1 << stage3_shift) - 1))
print("\treturn index;")
print("}")

print("static constexpr std::uint_least32_t ucd_caseless_sets[] = {")
print("UINT_LEAST32_MAX,")
for s in sets:
  s = sorted(s)
  for x in s:
    print('0x%04x,' % x, end=' ')
  print('UINT_LEAST32_MAX,')   
print('};')
print()
print("} // namespace lug::utf8")
print()
print("#endif")

