pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package smmintrin_h is

  -- Copyright (C) 2007-2017 Free Software Foundation, Inc.
  --   This file is part of GCC.
  --   GCC is free software; you can redistribute it and/or modify
  --   it under the terms of the GNU General Public License as published by
  --   the Free Software Foundation; either version 3, or (at your option)
  --   any later version.
  --   GCC is distributed in the hope that it will be useful,
  --   but WITHOUT ANY WARRANTY; without even the implied warranty of
  --   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  --   GNU General Public License for more details.
  --   Under Section 7 of GPL version 3, you are granted additional
  --   permissions described in the GCC Runtime Library Exception, version
  --   3.1, as published by the Free Software Foundation.
  --   You should have received a copy of the GNU General Public License and
  --   a copy of the GCC Runtime Library Exception along with this program;
  --   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
  --   <http://www.gnu.org/licenses/>.   

  -- Implemented from the specification included in the Intel C++ Compiler
  --   User Guide and Reference, version 10.0.   

  -- We need definitions from the SSSE3, SSE3, SSE2 and SSE header
  --   files.   

  -- Rounding mode macros.  
  -- Test Instruction  
  -- Packed integer 128-bit bitwise comparison. Return 1 if
  --   (__V & __M) == 0.   

   --  skipped func _mm_testz_si128

  -- Packed integer 128-bit bitwise comparison. Return 1 if
  --   (__V & ~__M) == 0.   

   --  skipped func _mm_testc_si128

  -- Packed integer 128-bit bitwise comparison. Return 1 if
  --   (__V & __M) != 0 && (__V & ~__M) != 0.   

   --  skipped func _mm_testnzc_si128

  -- Macros for packed integer 128-bit comparison intrinsics.   
  -- Packed/scalar double precision floating point rounding.   
  -- Packed/scalar single precision floating point rounding.   
  -- Macros for ceil/floor intrinsics.   
  -- SSE4.1  
  -- Integer blend instructions - select data from 2 sources using
  --   constant/variable mask.   

   --  skipped func _mm_blendv_epi8

  -- Single precision floating point blend instructions - select data
  --   from 2 sources using constant/variable mask.   

   --  skipped func _mm_blendv_ps

  -- Double precision floating point blend instructions - select data
  --   from 2 sources using constant/variable mask.   

   --  skipped func _mm_blendv_pd

  -- Dot product instructions with mask-defined summing and zeroing parts
  --   of result.   

  -- Packed integer 64-bit comparison, zeroing or filling with ones
  --   corresponding parts of result.   

   --  skipped func _mm_cmpeq_epi64

  --  Min/max packed integer instructions.   
   --  skipped func _mm_min_epi8

   --  skipped func _mm_max_epi8

   --  skipped func _mm_min_epu16

   --  skipped func _mm_max_epu16

   --  skipped func _mm_min_epi32

   --  skipped func _mm_max_epi32

   --  skipped func _mm_min_epu32

   --  skipped func _mm_max_epu32

  -- Packed integer 32-bit multiplication with truncation of upper
  --   halves of results.   

   --  skipped func _mm_mullo_epi32

  -- Packed integer 32-bit multiplication of 2 pairs of operands
  --   with two 64-bit results.   

   --  skipped func _mm_mul_epi32

  -- Insert single precision float into packed single precision array
  --   element selected by index N.  The bits [7-6] of N define S
  --   index, the bits [5-4] define D index, and bits [3-0] define
  --   zeroing mask for D.   

  -- Helper macro to create the N value for _mm_insert_ps.   
  -- Extract binary representation of single precision float from packed
  --   single precision array element of X selected by index N.   

  -- Extract binary representation of single precision float into
  --   D from packed single precision array element of S selected
  --   by index N.   

  -- Extract specified single precision float element into the lower
  --   part of __m128.   

  -- Insert integer, S, into packed integer array element of D
  --   selected by index N.   

  -- Extract integer from packed integer array element of X selected by
  --   index N.   

  -- Return horizontal packed word minimum and its index in bits [15:0]
  --   and bits [18:16] respectively.   

   --  skipped func _mm_minpos_epu16

  -- Packed integer sign-extension.   
   --  skipped func _mm_cvtepi8_epi32

   --  skipped func _mm_cvtepi16_epi32

   --  skipped func _mm_cvtepi8_epi64

   --  skipped func _mm_cvtepi32_epi64

   --  skipped func _mm_cvtepi16_epi64

   --  skipped func _mm_cvtepi8_epi16

  -- Packed integer zero-extension.  
   --  skipped func _mm_cvtepu8_epi32

   --  skipped func _mm_cvtepu16_epi32

   --  skipped func _mm_cvtepu8_epi64

   --  skipped func _mm_cvtepu32_epi64

   --  skipped func _mm_cvtepu16_epi64

   --  skipped func _mm_cvtepu8_epi16

  -- Pack 8 double words from 2 operands into 8 words of result with
  --   unsigned saturation.  

   --  skipped func _mm_packus_epi32

  -- Sum absolute 8-bit integer difference of adjacent groups of 4
  --   byte integers in the first 2 operands.  Starting offsets within
  --   operands are determined by the 3rd mask operand.   

  -- Load double quadword using non-temporal aligned hint.   
   --  skipped func _mm_stream_load_si128

  -- These macros specify the source data format.   
  -- These macros specify the comparison operation.   
  -- These macros specify the polarity.   
  -- These macros specify the output selection in _mm_cmpXstri ().   
  -- These macros specify the output selection in _mm_cmpXstrm ().   
  -- Intrinsics for text/string processing.   
  -- Intrinsics for text/string processing and reading values of
  --   EFlags.   

  -- Packed integer 64-bit comparison, zeroing or filling with ones
  --   corresponding parts of result.   

   --  skipped func _mm_cmpgt_epi64

  -- Accumulate CRC32 (polynomial 0x11EDC6F41) value.   
   --  skipped func _mm_crc32_u8

   --  skipped func _mm_crc32_u16

   --  skipped func _mm_crc32_u32

   --  skipped func _mm_crc32_u64

end smmintrin_h;
