pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package mmintrin_h is

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

  -- The Intel API is flexible enough that we must allow aliasing with other
  --   vector types, and their scalar components.   

   subtype uu_m64 is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\mmintrin.h:42

  -- Unaligned version of the same type   
   subtype uu_m64_u is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\mmintrin.h:45

  -- Internal data types for implementing the intrinsics.   
   subtype uu_v2si is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\mmintrin.h:48

   subtype uu_v4hi is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\mmintrin.h:49

   subtype uu_v8qi is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\mmintrin.h:50

   subtype uu_v1di is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\mmintrin.h:51

   subtype uu_v2sf is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\mmintrin.h:52

  -- Empty the multimedia state.   
   --  skipped func _mm_empty

   --  skipped func _m_empty

  -- Convert I to a __m64 object.  The integer is zero-extended to 64-bits.   
   --  skipped func _mm_cvtsi32_si64

   --  skipped func _m_from_int

  -- Convert I to a __m64 object.   
  -- Intel intrinsic.   
   --  skipped func _m_from_int64

   --  skipped func _mm_cvtsi64_m64

  -- Microsoft intrinsic.   
   --  skipped func _mm_cvtsi64x_si64

   --  skipped func _mm_set_pi64x

  -- Convert the lower 32 bits of the __m64 object into an integer.   
   --  skipped func _mm_cvtsi64_si32

   --  skipped func _m_to_int

  -- Convert the __m64 object to a 64bit integer.   
  -- Intel intrinsic.   
   --  skipped func _m_to_int64

   --  skipped func _mm_cvtm64_si64

  -- Microsoft intrinsic.   
   --  skipped func _mm_cvtsi64_si64x

  -- Pack the four 16-bit values from M1 into the lower four 8-bit values of
  --   the result, and the four 16-bit values from M2 into the upper four 8-bit
  --   values of the result, all with signed saturation.   

   --  skipped func _mm_packs_pi16

   --  skipped func _m_packsswb

  -- Pack the two 32-bit values from M1 in to the lower two 16-bit values of
  --   the result, and the two 32-bit values from M2 into the upper two 16-bit
  --   values of the result, all with signed saturation.   

   --  skipped func _mm_packs_pi32

   --  skipped func _m_packssdw

  -- Pack the four 16-bit values from M1 into the lower four 8-bit values of
  --   the result, and the four 16-bit values from M2 into the upper four 8-bit
  --   values of the result, all with unsigned saturation.   

   --  skipped func _mm_packs_pu16

   --  skipped func _m_packuswb

  -- Interleave the four 8-bit values from the high half of M1 with the four
  --   8-bit values from the high half of M2.   

   --  skipped func _mm_unpackhi_pi8

   --  skipped func _m_punpckhbw

  -- Interleave the two 16-bit values from the high half of M1 with the two
  --   16-bit values from the high half of M2.   

   --  skipped func _mm_unpackhi_pi16

   --  skipped func _m_punpckhwd

  -- Interleave the 32-bit value from the high half of M1 with the 32-bit
  --   value from the high half of M2.   

   --  skipped func _mm_unpackhi_pi32

   --  skipped func _m_punpckhdq

  -- Interleave the four 8-bit values from the low half of M1 with the four
  --   8-bit values from the low half of M2.   

   --  skipped func _mm_unpacklo_pi8

   --  skipped func _m_punpcklbw

  -- Interleave the two 16-bit values from the low half of M1 with the two
  --   16-bit values from the low half of M2.   

   --  skipped func _mm_unpacklo_pi16

   --  skipped func _m_punpcklwd

  -- Interleave the 32-bit value from the low half of M1 with the 32-bit
  --   value from the low half of M2.   

   --  skipped func _mm_unpacklo_pi32

   --  skipped func _m_punpckldq

  -- Add the 8-bit values in M1 to the 8-bit values in M2.   
   --  skipped func _mm_add_pi8

   --  skipped func _m_paddb

  -- Add the 16-bit values in M1 to the 16-bit values in M2.   
   --  skipped func _mm_add_pi16

   --  skipped func _m_paddw

  -- Add the 32-bit values in M1 to the 32-bit values in M2.   
   --  skipped func _mm_add_pi32

   --  skipped func _m_paddd

  -- Add the 64-bit values in M1 to the 64-bit values in M2.   
   --  skipped func _mm_add_si64

  -- Add the 8-bit values in M1 to the 8-bit values in M2 using signed
  --   saturated arithmetic.   

   --  skipped func _mm_adds_pi8

   --  skipped func _m_paddsb

  -- Add the 16-bit values in M1 to the 16-bit values in M2 using signed
  --   saturated arithmetic.   

   --  skipped func _mm_adds_pi16

   --  skipped func _m_paddsw

  -- Add the 8-bit values in M1 to the 8-bit values in M2 using unsigned
  --   saturated arithmetic.   

   --  skipped func _mm_adds_pu8

   --  skipped func _m_paddusb

  -- Add the 16-bit values in M1 to the 16-bit values in M2 using unsigned
  --   saturated arithmetic.   

   --  skipped func _mm_adds_pu16

   --  skipped func _m_paddusw

  -- Subtract the 8-bit values in M2 from the 8-bit values in M1.   
   --  skipped func _mm_sub_pi8

   --  skipped func _m_psubb

  -- Subtract the 16-bit values in M2 from the 16-bit values in M1.   
   --  skipped func _mm_sub_pi16

   --  skipped func _m_psubw

  -- Subtract the 32-bit values in M2 from the 32-bit values in M1.   
   --  skipped func _mm_sub_pi32

   --  skipped func _m_psubd

  -- Add the 64-bit values in M1 to the 64-bit values in M2.   
   --  skipped func _mm_sub_si64

  -- Subtract the 8-bit values in M2 from the 8-bit values in M1 using signed
  --   saturating arithmetic.   

   --  skipped func _mm_subs_pi8

   --  skipped func _m_psubsb

  -- Subtract the 16-bit values in M2 from the 16-bit values in M1 using
  --   signed saturating arithmetic.   

   --  skipped func _mm_subs_pi16

   --  skipped func _m_psubsw

  -- Subtract the 8-bit values in M2 from the 8-bit values in M1 using
  --   unsigned saturating arithmetic.   

   --  skipped func _mm_subs_pu8

   --  skipped func _m_psubusb

  -- Subtract the 16-bit values in M2 from the 16-bit values in M1 using
  --   unsigned saturating arithmetic.   

   --  skipped func _mm_subs_pu16

   --  skipped func _m_psubusw

  -- Multiply four 16-bit values in M1 by four 16-bit values in M2 producing
  --   four 32-bit intermediate results, which are then summed by pairs to
  --   produce two 32-bit results.   

   --  skipped func _mm_madd_pi16

   --  skipped func _m_pmaddwd

  -- Multiply four signed 16-bit values in M1 by four signed 16-bit values in
  --   M2 and produce the high 16 bits of the 32-bit results.   

   --  skipped func _mm_mulhi_pi16

   --  skipped func _m_pmulhw

  -- Multiply four 16-bit values in M1 by four 16-bit values in M2 and produce
  --   the low 16 bits of the results.   

   --  skipped func _mm_mullo_pi16

   --  skipped func _m_pmullw

  -- Shift four 16-bit values in M left by COUNT.   
   --  skipped func _mm_sll_pi16

   --  skipped func _m_psllw

   --  skipped func _mm_slli_pi16

   --  skipped func _m_psllwi

  -- Shift two 32-bit values in M left by COUNT.   
   --  skipped func _mm_sll_pi32

   --  skipped func _m_pslld

   --  skipped func _mm_slli_pi32

   --  skipped func _m_pslldi

  -- Shift the 64-bit value in M left by COUNT.   
   --  skipped func _mm_sll_si64

   --  skipped func _m_psllq

   --  skipped func _mm_slli_si64

   --  skipped func _m_psllqi

  -- Shift four 16-bit values in M right by COUNT; shift in the sign bit.   
   --  skipped func _mm_sra_pi16

   --  skipped func _m_psraw

   --  skipped func _mm_srai_pi16

   --  skipped func _m_psrawi

  -- Shift two 32-bit values in M right by COUNT; shift in the sign bit.   
   --  skipped func _mm_sra_pi32

   --  skipped func _m_psrad

   --  skipped func _mm_srai_pi32

   --  skipped func _m_psradi

  -- Shift four 16-bit values in M right by COUNT; shift in zeros.   
   --  skipped func _mm_srl_pi16

   --  skipped func _m_psrlw

   --  skipped func _mm_srli_pi16

   --  skipped func _m_psrlwi

  -- Shift two 32-bit values in M right by COUNT; shift in zeros.   
   --  skipped func _mm_srl_pi32

   --  skipped func _m_psrld

   --  skipped func _mm_srli_pi32

   --  skipped func _m_psrldi

  -- Shift the 64-bit value in M left by COUNT; shift in zeros.   
   --  skipped func _mm_srl_si64

   --  skipped func _m_psrlq

   --  skipped func _mm_srli_si64

   --  skipped func _m_psrlqi

  -- Bit-wise AND the 64-bit values in M1 and M2.   
   --  skipped func _mm_and_si64

   --  skipped func _m_pand

  -- Bit-wise complement the 64-bit value in M1 and bit-wise AND it with the
  --   64-bit value in M2.   

   --  skipped func _mm_andnot_si64

   --  skipped func _m_pandn

  -- Bit-wise inclusive OR the 64-bit values in M1 and M2.   
   --  skipped func _mm_or_si64

   --  skipped func _m_por

  -- Bit-wise exclusive OR the 64-bit values in M1 and M2.   
   --  skipped func _mm_xor_si64

   --  skipped func _m_pxor

  -- Compare eight 8-bit values.  The result of the comparison is 0xFF if the
  --   test is true and zero if false.   

   --  skipped func _mm_cmpeq_pi8

   --  skipped func _m_pcmpeqb

   --  skipped func _mm_cmpgt_pi8

   --  skipped func _m_pcmpgtb

  -- Compare four 16-bit values.  The result of the comparison is 0xFFFF if
  --   the test is true and zero if false.   

   --  skipped func _mm_cmpeq_pi16

   --  skipped func _m_pcmpeqw

   --  skipped func _mm_cmpgt_pi16

   --  skipped func _m_pcmpgtw

  -- Compare two 32-bit values.  The result of the comparison is 0xFFFFFFFF if
  --   the test is true and zero if false.   

   --  skipped func _mm_cmpeq_pi32

   --  skipped func _m_pcmpeqd

   --  skipped func _mm_cmpgt_pi32

   --  skipped func _m_pcmpgtd

  -- Creates a 64-bit zero.   
   --  skipped func _mm_setzero_si64

  -- Creates a vector of two 32-bit values; I0 is least significant.   
   --  skipped func _mm_set_pi32

  -- Creates a vector of four 16-bit values; W0 is least significant.   
   --  skipped func _mm_set_pi16

  -- Creates a vector of eight 8-bit values; B0 is least significant.   
   --  skipped func _mm_set_pi8

  -- Similar, but with the arguments in reverse order.   
   --  skipped func _mm_setr_pi32

   --  skipped func _mm_setr_pi16

   --  skipped func _mm_setr_pi8

  -- Creates a vector of two 32-bit values, both elements containing I.   
   --  skipped func _mm_set1_pi32

  -- Creates a vector of four 16-bit values, all elements containing W.   
   --  skipped func _mm_set1_pi16

  -- Creates a vector of eight 8-bit values, all elements containing B.   
   --  skipped func _mm_set1_pi8

end mmintrin_h;
