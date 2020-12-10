pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package xmmintrin_h is

  -- Copyright (C) 2002-2017 Free Software Foundation, Inc.
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
  --   User Guide and Reference, version 9.0.   

  -- We need type definitions from the MMX header file.   
  -- Get _mm_malloc () and _mm_free ().   
  -- Constants for use with _mm_prefetch.   
   subtype u_mm_hint is unsigned;
   u_MM_HINT_ET0 : constant unsigned := 7;
   u_MM_HINT_ET1 : constant unsigned := 6;
   u_MM_HINT_T0 : constant unsigned := 3;
   u_MM_HINT_T1 : constant unsigned := 2;
   u_MM_HINT_T2 : constant unsigned := 1;
   u_MM_HINT_NTA : constant unsigned := 0;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\xmmintrin.h:37

  -- _MM_HINT_ET is _MM_HINT_T with set 3rd bit.   
  -- Loads one cache line from address P to a location "closer" to the
  --   processor.  The selector I specifies the type of prefetch operation.   

  -- The Intel API is flexible enough that we must allow aliasing with other
  --   vector types, and their scalar components.   

   subtype uu_m128 is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\xmmintrin.h:69

  -- Unaligned version of the same type.   
   subtype uu_m128_u is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\xmmintrin.h:72

  -- Internal data types for implementing the intrinsics.   
   subtype uu_v4sf is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\xmmintrin.h:75

  -- Create a selector for use with the SHUFPS instruction.   
  -- Bits in the MXCSR.   
  -- Create an undefined vector.   
   --  skipped func _mm_undefined_ps

  -- Create a vector of zeros.   
   --  skipped func _mm_setzero_ps

  -- Perform the respective operation on the lower SPFP (single-precision
  --   floating-point) values of A and B; the upper three SPFP values are
  --   passed through from A.   

   --  skipped func _mm_add_ss

   --  skipped func _mm_sub_ss

   --  skipped func _mm_mul_ss

   --  skipped func _mm_div_ss

   --  skipped func _mm_sqrt_ss

   --  skipped func _mm_rcp_ss

   --  skipped func _mm_rsqrt_ss

   --  skipped func _mm_min_ss

   --  skipped func _mm_max_ss

  -- Perform the respective operation on the four SPFP values in A and B.   
   --  skipped func _mm_add_ps

   --  skipped func _mm_sub_ps

   --  skipped func _mm_mul_ps

   --  skipped func _mm_div_ps

   --  skipped func _mm_sqrt_ps

   --  skipped func _mm_rcp_ps

   --  skipped func _mm_rsqrt_ps

   --  skipped func _mm_min_ps

   --  skipped func _mm_max_ps

  -- Perform logical bit-wise operations on 128-bit values.   
   --  skipped func _mm_and_ps

   --  skipped func _mm_andnot_ps

   --  skipped func _mm_or_ps

   --  skipped func _mm_xor_ps

  -- Perform a comparison on the lower SPFP values of A and B.  If the
  --   comparison is true, place a mask of all ones in the result, otherwise a
  --   mask of zeros.  The upper three SPFP values are passed through from A.   

   --  skipped func _mm_cmpeq_ss

   --  skipped func _mm_cmplt_ss

   --  skipped func _mm_cmple_ss

   --  skipped func _mm_cmpgt_ss

   --  skipped func _mm_cmpge_ss

   --  skipped func _mm_cmpneq_ss

   --  skipped func _mm_cmpnlt_ss

   --  skipped func _mm_cmpnle_ss

   --  skipped func _mm_cmpngt_ss

   --  skipped func _mm_cmpnge_ss

   --  skipped func _mm_cmpord_ss

   --  skipped func _mm_cmpunord_ss

  -- Perform a comparison on the four SPFP values of A and B.  For each
  --   element, if the comparison is true, place a mask of all ones in the
  --   result, otherwise a mask of zeros.   

   --  skipped func _mm_cmpeq_ps

   --  skipped func _mm_cmplt_ps

   --  skipped func _mm_cmple_ps

   --  skipped func _mm_cmpgt_ps

   --  skipped func _mm_cmpge_ps

   --  skipped func _mm_cmpneq_ps

   --  skipped func _mm_cmpnlt_ps

   --  skipped func _mm_cmpnle_ps

   --  skipped func _mm_cmpngt_ps

   --  skipped func _mm_cmpnge_ps

   --  skipped func _mm_cmpord_ps

   --  skipped func _mm_cmpunord_ps

  -- Compare the lower SPFP values of A and B and return 1 if true
  --   and 0 if false.   

   --  skipped func _mm_comieq_ss

   --  skipped func _mm_comilt_ss

   --  skipped func _mm_comile_ss

   --  skipped func _mm_comigt_ss

   --  skipped func _mm_comige_ss

   --  skipped func _mm_comineq_ss

   --  skipped func _mm_ucomieq_ss

   --  skipped func _mm_ucomilt_ss

   --  skipped func _mm_ucomile_ss

   --  skipped func _mm_ucomigt_ss

   --  skipped func _mm_ucomige_ss

   --  skipped func _mm_ucomineq_ss

  -- Convert the lower SPFP value to a 32-bit integer according to the current
  --   rounding mode.   

   --  skipped func _mm_cvtss_si32

   --  skipped func _mm_cvt_ss2si

  -- Convert the lower SPFP value to a 32-bit integer according to the
  --   current rounding mode.   

  -- Intel intrinsic.   
   --  skipped func _mm_cvtss_si64

  -- Microsoft intrinsic.   
   --  skipped func _mm_cvtss_si64x

  -- Convert the two lower SPFP values to 32-bit integers according to the
  --   current rounding mode.  Return the integers in packed form.   

   --  skipped func _mm_cvtps_pi32

   --  skipped func _mm_cvt_ps2pi

  -- Truncate the lower SPFP value to a 32-bit integer.   
   --  skipped func _mm_cvttss_si32

   --  skipped func _mm_cvtt_ss2si

  -- Truncate the lower SPFP value to a 32-bit integer.   
  -- Intel intrinsic.   
   --  skipped func _mm_cvttss_si64

  -- Microsoft intrinsic.   
   --  skipped func _mm_cvttss_si64x

  -- Truncate the two lower SPFP values to 32-bit integers.  Return the
  --   integers in packed form.   

   --  skipped func _mm_cvttps_pi32

   --  skipped func _mm_cvtt_ps2pi

  -- Convert B to a SPFP value and insert it as element zero in A.   
   --  skipped func _mm_cvtsi32_ss

   --  skipped func _mm_cvt_si2ss

  -- Convert B to a SPFP value and insert it as element zero in A.   
  -- Intel intrinsic.   
   --  skipped func _mm_cvtsi64_ss

  -- Microsoft intrinsic.   
   --  skipped func _mm_cvtsi64x_ss

  -- Convert the two 32-bit values in B to SPFP form and insert them
  --   as the two lower elements in A.   

   --  skipped func _mm_cvtpi32_ps

   --  skipped func _mm_cvt_pi2ps

  -- Convert the four signed 16-bit values in A to SPFP form.   
   --  skipped func _mm_cvtpi16_ps

  -- This comparison against zero gives us a mask that can be used to
  --     fill in the missing sign bits in the unpack operations below, so
  --     that we get signed values after unpacking.   

  -- Convert the four words to doublewords.   
  -- Convert the doublewords to floating point two at a time.   
  -- Convert the four unsigned 16-bit values in A to SPFP form.   
   --  skipped func _mm_cvtpu16_ps

  -- Convert the four words to doublewords.   
  -- Convert the doublewords to floating point two at a time.   
  -- Convert the low four signed 8-bit values in A to SPFP form.   
   --  skipped func _mm_cvtpi8_ps

  -- This comparison against zero gives us a mask that can be used to
  --     fill in the missing sign bits in the unpack operations below, so
  --     that we get signed values after unpacking.   

  -- Convert the four low bytes to words.   
  -- Convert the low four unsigned 8-bit values in A to SPFP form.   
   --  skipped func _mm_cvtpu8_ps

  -- Convert the four signed 32-bit values in A and B to SPFP form.   
   --  skipped func _mm_cvtpi32x2_ps

  -- Convert the four SPFP values in A to four signed 16-bit integers.   
   --  skipped func _mm_cvtps_pi16

  -- Convert the four SPFP values in A to four signed 8-bit integers.   
   --  skipped func _mm_cvtps_pi8

  -- Selects four specific SPFP values from A and B based on MASK.   
  -- Selects and interleaves the upper two SPFP values from A and B.   
   --  skipped func _mm_unpackhi_ps

  -- Selects and interleaves the lower two SPFP values from A and B.   
   --  skipped func _mm_unpacklo_ps

  -- Sets the upper two SPFP values with 64-bits of data loaded from P;
  --   the lower two values are passed through from A.   

   --  skipped func _mm_loadh_pi

  -- Stores the upper two SPFP values of A into P.   
   --  skipped func _mm_storeh_pi

  -- Moves the upper two values of B into the lower two values of A.   
   --  skipped func _mm_movehl_ps

  -- Moves the lower two values of B into the upper two values of A.   
   --  skipped func _mm_movelh_ps

  -- Sets the lower two SPFP values with 64-bits of data loaded from P;
  --   the upper two values are passed through from A.   

   --  skipped func _mm_loadl_pi

  -- Stores the lower two SPFP values of A into P.   
   --  skipped func _mm_storel_pi

  -- Creates a 4-bit mask from the most significant bits of the SPFP values.   
   --  skipped func _mm_movemask_ps

  -- Return the contents of the control register.   
   --  skipped func _mm_getcsr

  -- Read exception bits from the control register.   
   --  skipped func _MM_GET_EXCEPTION_STATE

   --  skipped func _MM_GET_EXCEPTION_MASK

   --  skipped func _MM_GET_ROUNDING_MODE

   --  skipped func _MM_GET_FLUSH_ZERO_MODE

  -- Set the control register to I.   
   --  skipped func _mm_setcsr

  -- Set exception bits in the control register.   
   --  skipped func _MM_SET_EXCEPTION_STATE

   --  skipped func _MM_SET_EXCEPTION_MASK

   --  skipped func _MM_SET_ROUNDING_MODE

   --  skipped func _MM_SET_FLUSH_ZERO_MODE

  -- Create a vector with element 0 as F and the rest zero.   
   --  skipped func _mm_set_ss

  -- Create a vector with all four elements equal to F.   
   --  skipped func _mm_set1_ps

   --  skipped func _mm_set_ps1

  -- Create a vector with element 0 as *P and the rest zero.   
   --  skipped func _mm_load_ss

  -- Create a vector with all four elements equal to *P.   
   --  skipped func _mm_load1_ps

   --  skipped func _mm_load_ps1

  -- Load four SPFP values from P.  The address must be 16-byte aligned.   
   --  skipped func _mm_load_ps

  -- Load four SPFP values from P.  The address need not be 16-byte aligned.   
   --  skipped func _mm_loadu_ps

  -- Load four SPFP values in reverse order.  The address must be aligned.   
   --  skipped func _mm_loadr_ps

  -- Create the vector [Z Y X W].   
   --  skipped func _mm_set_ps

  -- Create the vector [W X Y Z].   
   --  skipped func _mm_setr_ps

  -- Stores the lower SPFP value.   
   --  skipped func _mm_store_ss

   --  skipped func _mm_cvtss_f32

  -- Store four SPFP values.  The address must be 16-byte aligned.   
   --  skipped func _mm_store_ps

  -- Store four SPFP values.  The address need not be 16-byte aligned.   
   --  skipped func _mm_storeu_ps

  -- Store the lower SPFP value across four words.   
   --  skipped func _mm_store1_ps

   --  skipped func _mm_store_ps1

  -- Store four SPFP values in reverse order.  The address must be aligned.   
   --  skipped func _mm_storer_ps

  -- Sets the low SPFP value of A from the low value of B.   
   --  skipped func _mm_move_ss

  -- Extracts one of the four words of A.  The selector N must be immediate.   
  -- Inserts word D into one of four words of A.  The selector N must be
  --   immediate.   

  -- Compute the element-wise maximum of signed 16-bit values.   
   --  skipped func _mm_max_pi16

   --  skipped func _m_pmaxsw

  -- Compute the element-wise maximum of unsigned 8-bit values.   
   --  skipped func _mm_max_pu8

   --  skipped func _m_pmaxub

  -- Compute the element-wise minimum of signed 16-bit values.   
   --  skipped func _mm_min_pi16

   --  skipped func _m_pminsw

  -- Compute the element-wise minimum of unsigned 8-bit values.   
   --  skipped func _mm_min_pu8

   --  skipped func _m_pminub

  -- Create an 8-bit mask of the signs of 8-bit values.   
   --  skipped func _mm_movemask_pi8

   --  skipped func _m_pmovmskb

  -- Multiply four unsigned 16-bit values in A by four unsigned 16-bit values
  --   in B and produce the high 16 bits of the 32-bit results.   

   --  skipped func _mm_mulhi_pu16

   --  skipped func _m_pmulhuw

  -- Return a combination of the four 16-bit values in A.  The selector
  --   must be an immediate.   

  -- Conditionally store byte elements of A into P.  The high bit of each
  --   byte in the selector N determines whether the corresponding byte from
  --   A is stored.   

   --  skipped func _mm_maskmove_si64

   --  skipped func _m_maskmovq

  -- Compute the rounded averages of the unsigned 8-bit values in A and B.   
   --  skipped func _mm_avg_pu8

   --  skipped func _m_pavgb

  -- Compute the rounded averages of the unsigned 16-bit values in A and B.   
   --  skipped func _mm_avg_pu16

   --  skipped func _m_pavgw

  -- Compute the sum of the absolute differences of the unsigned 8-bit
  --   values in A and B.  Return the value in the lower 16-bit word; the
  --   upper words are cleared.   

   --  skipped func _mm_sad_pu8

   --  skipped func _m_psadbw

  -- Stores the data in A to the address P without polluting the caches.   
   --  skipped func _mm_stream_pi

  -- Likewise.  The address must be 16-byte aligned.   
   --  skipped func _mm_stream_ps

  -- Guarantees that every preceding store is globally visible before
  --   any subsequent store.   

   --  skipped func _mm_sfence

  -- Transpose the 4x4 matrix composed of row[0-3].   
  -- For backward source compatibility.   
  -- The execution of the next instruction is delayed by an implementation
  --   specific amount of time.  The instruction does not modify the
  --   architectural state.  This is after the pop_options pragma because
  --   it does not require SSE support in the processor--the encoding is a
  --   nop on processors that do not support it.   

   --  skipped func _mm_pause

end xmmintrin_h;
