pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package avx512fintrin_h is

  -- Copyright (C) 2013-2017 Free Software Foundation, Inc.
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

  -- Internal data types for implementing the intrinsics.   
   subtype uu_v8du is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avx512fintrin.h:41

   subtype uu_v16su is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avx512fintrin.h:43

   subtype uu_v32hu is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avx512fintrin.h:45

   subtype uu_v64qu is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avx512fintrin.h:47

  -- The Intel API is flexible enough that we must allow aliasing with other
  --   vector types, and their scalar components.   

  -- Unaligned version of the same type.   
   subtype uu_m512_u is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avx512fintrin.h:56

   subtype uu_m512i_u is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avx512fintrin.h:57

   subtype uu_m512d_u is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avx512fintrin.h:58

   --  skipped func _mm512_int2mask

   --  skipped func _mm512_mask2int

   --  skipped func _mm512_set_epi64

  -- Create the vector [A B C D E F G H I J K L M N O P].   
   --  skipped func _mm512_set_epi32

   --  skipped func _mm512_set_pd

   --  skipped func _mm512_set_ps

   --  skipped func _mm512_undefined_ps

   --  skipped func _mm512_undefined_pd

   --  skipped func _mm512_undefined_epi32

   --  skipped func _mm512_set1_epi8

   --  skipped func _mm512_set1_epi16

   --  skipped func _mm512_set1_pd

   --  skipped func _mm512_set1_ps

  -- Create the vector [A B C D A B C D A B C D A B C D].   
   --  skipped func _mm512_set4_epi32

   --  skipped func _mm512_set4_epi64

   --  skipped func _mm512_set4_pd

   --  skipped func _mm512_set4_ps

   --  skipped func _mm512_setzero_ps

   --  skipped func _mm512_setzero_pd

   --  skipped func _mm512_setzero_epi32

   --  skipped func _mm512_setzero_si512

   --  skipped func _mm512_mask_mov_pd

   --  skipped func _mm512_maskz_mov_pd

   --  skipped func _mm512_mask_mov_ps

   --  skipped func _mm512_maskz_mov_ps

   --  skipped func _mm512_load_pd

   --  skipped func _mm512_mask_load_pd

   --  skipped func _mm512_maskz_load_pd

   --  skipped func _mm512_store_pd

   --  skipped func _mm512_mask_store_pd

   --  skipped func _mm512_load_ps

   --  skipped func _mm512_mask_load_ps

   --  skipped func _mm512_maskz_load_ps

   --  skipped func _mm512_store_ps

   --  skipped func _mm512_mask_store_ps

   --  skipped func _mm512_mask_mov_epi64

   --  skipped func _mm512_maskz_mov_epi64

   --  skipped func _mm512_load_epi64

   --  skipped func _mm512_mask_load_epi64

   --  skipped func _mm512_maskz_load_epi64

   --  skipped func _mm512_store_epi64

   --  skipped func _mm512_mask_store_epi64

   --  skipped func _mm512_mask_mov_epi32

   --  skipped func _mm512_maskz_mov_epi32

   --  skipped func _mm512_load_si512

   --  skipped func _mm512_load_epi32

   --  skipped func _mm512_mask_load_epi32

   --  skipped func _mm512_maskz_load_epi32

   --  skipped func _mm512_store_si512

   --  skipped func _mm512_store_epi32

   --  skipped func _mm512_mask_store_epi32

   --  skipped func _mm512_mullo_epi32

   --  skipped func _mm512_maskz_mullo_epi32

   --  skipped func _mm512_mask_mullo_epi32

   --  skipped func _mm512_sllv_epi32

   --  skipped func _mm512_mask_sllv_epi32

   --  skipped func _mm512_maskz_sllv_epi32

   --  skipped func _mm512_srav_epi32

   --  skipped func _mm512_mask_srav_epi32

   --  skipped func _mm512_maskz_srav_epi32

   --  skipped func _mm512_srlv_epi32

   --  skipped func _mm512_mask_srlv_epi32

   --  skipped func _mm512_maskz_srlv_epi32

   --  skipped func _mm512_add_epi64

   --  skipped func _mm512_mask_add_epi64

   --  skipped func _mm512_maskz_add_epi64

   --  skipped func _mm512_sub_epi64

   --  skipped func _mm512_mask_sub_epi64

   --  skipped func _mm512_maskz_sub_epi64

   --  skipped func _mm512_sllv_epi64

   --  skipped func _mm512_mask_sllv_epi64

   --  skipped func _mm512_maskz_sllv_epi64

   --  skipped func _mm512_srav_epi64

   --  skipped func _mm512_mask_srav_epi64

   --  skipped func _mm512_maskz_srav_epi64

   --  skipped func _mm512_srlv_epi64

   --  skipped func _mm512_mask_srlv_epi64

   --  skipped func _mm512_maskz_srlv_epi64

   --  skipped func _mm512_add_epi32

   --  skipped func _mm512_mask_add_epi32

   --  skipped func _mm512_maskz_add_epi32

   --  skipped func _mm512_mul_epi32

   --  skipped func _mm512_mask_mul_epi32

   --  skipped func _mm512_maskz_mul_epi32

   --  skipped func _mm512_sub_epi32

   --  skipped func _mm512_mask_sub_epi32

   --  skipped func _mm512_maskz_sub_epi32

   --  skipped func _mm512_mul_epu32

   --  skipped func _mm512_mask_mul_epu32

   --  skipped func _mm512_maskz_mul_epu32

   --  skipped func _mm512_sll_epi64

   --  skipped func _mm512_mask_sll_epi64

   --  skipped func _mm512_maskz_sll_epi64

   --  skipped func _mm512_srl_epi64

   --  skipped func _mm512_mask_srl_epi64

   --  skipped func _mm512_maskz_srl_epi64

   --  skipped func _mm512_sra_epi64

   --  skipped func _mm512_mask_sra_epi64

   --  skipped func _mm512_maskz_sra_epi64

   --  skipped func _mm512_sll_epi32

   --  skipped func _mm512_mask_sll_epi32

   --  skipped func _mm512_maskz_sll_epi32

   --  skipped func _mm512_srl_epi32

   --  skipped func _mm512_mask_srl_epi32

   --  skipped func _mm512_maskz_srl_epi32

   --  skipped func _mm512_sra_epi32

   --  skipped func _mm512_mask_sra_epi32

   --  skipped func _mm512_maskz_sra_epi32

   --  skipped func _mm512_rcp14_pd

   --  skipped func _mm512_mask_rcp14_pd

   --  skipped func _mm512_maskz_rcp14_pd

   --  skipped func _mm512_rcp14_ps

   --  skipped func _mm512_mask_rcp14_ps

   --  skipped func _mm512_maskz_rcp14_ps

   --  skipped func _mm_rcp14_sd

   --  skipped func _mm_rcp14_ss

   --  skipped func _mm512_rsqrt14_pd

   --  skipped func _mm512_mask_rsqrt14_pd

   --  skipped func _mm512_maskz_rsqrt14_pd

   --  skipped func _mm512_rsqrt14_ps

   --  skipped func _mm512_mask_rsqrt14_ps

   --  skipped func _mm512_maskz_rsqrt14_ps

   --  skipped func _mm_rsqrt14_sd

   --  skipped func _mm_rsqrt14_ss

   --  skipped func _mm512_cvtepi8_epi32

   --  skipped func _mm512_mask_cvtepi8_epi32

   --  skipped func _mm512_maskz_cvtepi8_epi32

   --  skipped func _mm512_cvtepi8_epi64

   --  skipped func _mm512_mask_cvtepi8_epi64

   --  skipped func _mm512_maskz_cvtepi8_epi64

   --  skipped func _mm512_cvtepi16_epi32

   --  skipped func _mm512_mask_cvtepi16_epi32

   --  skipped func _mm512_maskz_cvtepi16_epi32

   --  skipped func _mm512_cvtepi16_epi64

   --  skipped func _mm512_mask_cvtepi16_epi64

   --  skipped func _mm512_maskz_cvtepi16_epi64

   --  skipped func _mm512_cvtepi32_epi64

   --  skipped func _mm512_mask_cvtepi32_epi64

   --  skipped func _mm512_maskz_cvtepi32_epi64

   --  skipped func _mm512_cvtepu8_epi32

   --  skipped func _mm512_mask_cvtepu8_epi32

   --  skipped func _mm512_maskz_cvtepu8_epi32

   --  skipped func _mm512_cvtepu8_epi64

   --  skipped func _mm512_mask_cvtepu8_epi64

   --  skipped func _mm512_maskz_cvtepu8_epi64

   --  skipped func _mm512_cvtepu16_epi32

   --  skipped func _mm512_mask_cvtepu16_epi32

   --  skipped func _mm512_maskz_cvtepu16_epi32

   --  skipped func _mm512_cvtepu16_epi64

   --  skipped func _mm512_mask_cvtepu16_epi64

   --  skipped func _mm512_maskz_cvtepu16_epi64

   --  skipped func _mm512_cvtepu32_epi64

   --  skipped func _mm512_mask_cvtepu32_epi64

   --  skipped func _mm512_maskz_cvtepu32_epi64

   --  skipped func _mm512_abs_epi64

   --  skipped func _mm512_mask_abs_epi64

   --  skipped func _mm512_maskz_abs_epi64

   --  skipped func _mm512_abs_epi32

   --  skipped func _mm512_mask_abs_epi32

   --  skipped func _mm512_maskz_abs_epi32

   --  skipped func _mm512_broadcastss_ps

   --  skipped func _mm512_mask_broadcastss_ps

   --  skipped func _mm512_maskz_broadcastss_ps

   --  skipped func _mm512_broadcastsd_pd

   --  skipped func _mm512_mask_broadcastsd_pd

   --  skipped func _mm512_maskz_broadcastsd_pd

   --  skipped func _mm512_broadcastd_epi32

   --  skipped func _mm512_mask_broadcastd_epi32

   --  skipped func _mm512_maskz_broadcastd_epi32

   --  skipped func _mm512_set1_epi32

   --  skipped func _mm512_mask_set1_epi32

   --  skipped func _mm512_maskz_set1_epi32

   --  skipped func _mm512_broadcastq_epi64

   --  skipped func _mm512_mask_broadcastq_epi64

   --  skipped func _mm512_maskz_broadcastq_epi64

   --  skipped func _mm512_set1_epi64

   --  skipped func _mm512_mask_set1_epi64

   --  skipped func _mm512_maskz_set1_epi64

   --  skipped func _mm512_broadcast_f32x4

   --  skipped func _mm512_mask_broadcast_f32x4

   --  skipped func _mm512_maskz_broadcast_f32x4

   --  skipped func _mm512_broadcast_i32x4

   --  skipped func _mm512_mask_broadcast_i32x4

   --  skipped func _mm512_maskz_broadcast_i32x4

   --  skipped func _mm512_broadcast_f64x4

   --  skipped func _mm512_mask_broadcast_f64x4

   --  skipped func _mm512_maskz_broadcast_f64x4

   --  skipped func _mm512_broadcast_i64x4

   --  skipped func _mm512_mask_broadcast_i64x4

   --  skipped func _mm512_maskz_broadcast_i64x4

   type u_MM_PERM_ENUM is 
     (u_MM_PERM_AAAA,
      u_MM_PERM_AAAB,
      u_MM_PERM_AAAC,
      u_MM_PERM_AAAD,
      u_MM_PERM_AABA,
      u_MM_PERM_AABB,
      u_MM_PERM_AABC,
      u_MM_PERM_AABD,
      u_MM_PERM_AACA,
      u_MM_PERM_AACB,
      u_MM_PERM_AACC,
      u_MM_PERM_AACD,
      u_MM_PERM_AADA,
      u_MM_PERM_AADB,
      u_MM_PERM_AADC,
      u_MM_PERM_AADD,
      u_MM_PERM_ABAA,
      u_MM_PERM_ABAB,
      u_MM_PERM_ABAC,
      u_MM_PERM_ABAD,
      u_MM_PERM_ABBA,
      u_MM_PERM_ABBB,
      u_MM_PERM_ABBC,
      u_MM_PERM_ABBD,
      u_MM_PERM_ABCA,
      u_MM_PERM_ABCB,
      u_MM_PERM_ABCC,
      u_MM_PERM_ABCD,
      u_MM_PERM_ABDA,
      u_MM_PERM_ABDB,
      u_MM_PERM_ABDC,
      u_MM_PERM_ABDD,
      u_MM_PERM_ACAA,
      u_MM_PERM_ACAB,
      u_MM_PERM_ACAC,
      u_MM_PERM_ACAD,
      u_MM_PERM_ACBA,
      u_MM_PERM_ACBB,
      u_MM_PERM_ACBC,
      u_MM_PERM_ACBD,
      u_MM_PERM_ACCA,
      u_MM_PERM_ACCB,
      u_MM_PERM_ACCC,
      u_MM_PERM_ACCD,
      u_MM_PERM_ACDA,
      u_MM_PERM_ACDB,
      u_MM_PERM_ACDC,
      u_MM_PERM_ACDD,
      u_MM_PERM_ADAA,
      u_MM_PERM_ADAB,
      u_MM_PERM_ADAC,
      u_MM_PERM_ADAD,
      u_MM_PERM_ADBA,
      u_MM_PERM_ADBB,
      u_MM_PERM_ADBC,
      u_MM_PERM_ADBD,
      u_MM_PERM_ADCA,
      u_MM_PERM_ADCB,
      u_MM_PERM_ADCC,
      u_MM_PERM_ADCD,
      u_MM_PERM_ADDA,
      u_MM_PERM_ADDB,
      u_MM_PERM_ADDC,
      u_MM_PERM_ADDD,
      u_MM_PERM_BAAA,
      u_MM_PERM_BAAB,
      u_MM_PERM_BAAC,
      u_MM_PERM_BAAD,
      u_MM_PERM_BABA,
      u_MM_PERM_BABB,
      u_MM_PERM_BABC,
      u_MM_PERM_BABD,
      u_MM_PERM_BACA,
      u_MM_PERM_BACB,
      u_MM_PERM_BACC,
      u_MM_PERM_BACD,
      u_MM_PERM_BADA,
      u_MM_PERM_BADB,
      u_MM_PERM_BADC,
      u_MM_PERM_BADD,
      u_MM_PERM_BBAA,
      u_MM_PERM_BBAB,
      u_MM_PERM_BBAC,
      u_MM_PERM_BBAD,
      u_MM_PERM_BBBA,
      u_MM_PERM_BBBB,
      u_MM_PERM_BBBC,
      u_MM_PERM_BBBD,
      u_MM_PERM_BBCA,
      u_MM_PERM_BBCB,
      u_MM_PERM_BBCC,
      u_MM_PERM_BBCD,
      u_MM_PERM_BBDA,
      u_MM_PERM_BBDB,
      u_MM_PERM_BBDC,
      u_MM_PERM_BBDD,
      u_MM_PERM_BCAA,
      u_MM_PERM_BCAB,
      u_MM_PERM_BCAC,
      u_MM_PERM_BCAD,
      u_MM_PERM_BCBA,
      u_MM_PERM_BCBB,
      u_MM_PERM_BCBC,
      u_MM_PERM_BCBD,
      u_MM_PERM_BCCA,
      u_MM_PERM_BCCB,
      u_MM_PERM_BCCC,
      u_MM_PERM_BCCD,
      u_MM_PERM_BCDA,
      u_MM_PERM_BCDB,
      u_MM_PERM_BCDC,
      u_MM_PERM_BCDD,
      u_MM_PERM_BDAA,
      u_MM_PERM_BDAB,
      u_MM_PERM_BDAC,
      u_MM_PERM_BDAD,
      u_MM_PERM_BDBA,
      u_MM_PERM_BDBB,
      u_MM_PERM_BDBC,
      u_MM_PERM_BDBD,
      u_MM_PERM_BDCA,
      u_MM_PERM_BDCB,
      u_MM_PERM_BDCC,
      u_MM_PERM_BDCD,
      u_MM_PERM_BDDA,
      u_MM_PERM_BDDB,
      u_MM_PERM_BDDC,
      u_MM_PERM_BDDD,
      u_MM_PERM_CAAA,
      u_MM_PERM_CAAB,
      u_MM_PERM_CAAC,
      u_MM_PERM_CAAD,
      u_MM_PERM_CABA,
      u_MM_PERM_CABB,
      u_MM_PERM_CABC,
      u_MM_PERM_CABD,
      u_MM_PERM_CACA,
      u_MM_PERM_CACB,
      u_MM_PERM_CACC,
      u_MM_PERM_CACD,
      u_MM_PERM_CADA,
      u_MM_PERM_CADB,
      u_MM_PERM_CADC,
      u_MM_PERM_CADD,
      u_MM_PERM_CBAA,
      u_MM_PERM_CBAB,
      u_MM_PERM_CBAC,
      u_MM_PERM_CBAD,
      u_MM_PERM_CBBA,
      u_MM_PERM_CBBB,
      u_MM_PERM_CBBC,
      u_MM_PERM_CBBD,
      u_MM_PERM_CBCA,
      u_MM_PERM_CBCB,
      u_MM_PERM_CBCC,
      u_MM_PERM_CBCD,
      u_MM_PERM_CBDA,
      u_MM_PERM_CBDB,
      u_MM_PERM_CBDC,
      u_MM_PERM_CBDD,
      u_MM_PERM_CCAA,
      u_MM_PERM_CCAB,
      u_MM_PERM_CCAC,
      u_MM_PERM_CCAD,
      u_MM_PERM_CCBA,
      u_MM_PERM_CCBB,
      u_MM_PERM_CCBC,
      u_MM_PERM_CCBD,
      u_MM_PERM_CCCA,
      u_MM_PERM_CCCB,
      u_MM_PERM_CCCC,
      u_MM_PERM_CCCD,
      u_MM_PERM_CCDA,
      u_MM_PERM_CCDB,
      u_MM_PERM_CCDC,
      u_MM_PERM_CCDD,
      u_MM_PERM_CDAA,
      u_MM_PERM_CDAB,
      u_MM_PERM_CDAC,
      u_MM_PERM_CDAD,
      u_MM_PERM_CDBA,
      u_MM_PERM_CDBB,
      u_MM_PERM_CDBC,
      u_MM_PERM_CDBD,
      u_MM_PERM_CDCA,
      u_MM_PERM_CDCB,
      u_MM_PERM_CDCC,
      u_MM_PERM_CDCD,
      u_MM_PERM_CDDA,
      u_MM_PERM_CDDB,
      u_MM_PERM_CDDC,
      u_MM_PERM_CDDD,
      u_MM_PERM_DAAA,
      u_MM_PERM_DAAB,
      u_MM_PERM_DAAC,
      u_MM_PERM_DAAD,
      u_MM_PERM_DABA,
      u_MM_PERM_DABB,
      u_MM_PERM_DABC,
      u_MM_PERM_DABD,
      u_MM_PERM_DACA,
      u_MM_PERM_DACB,
      u_MM_PERM_DACC,
      u_MM_PERM_DACD,
      u_MM_PERM_DADA,
      u_MM_PERM_DADB,
      u_MM_PERM_DADC,
      u_MM_PERM_DADD,
      u_MM_PERM_DBAA,
      u_MM_PERM_DBAB,
      u_MM_PERM_DBAC,
      u_MM_PERM_DBAD,
      u_MM_PERM_DBBA,
      u_MM_PERM_DBBB,
      u_MM_PERM_DBBC,
      u_MM_PERM_DBBD,
      u_MM_PERM_DBCA,
      u_MM_PERM_DBCB,
      u_MM_PERM_DBCC,
      u_MM_PERM_DBCD,
      u_MM_PERM_DBDA,
      u_MM_PERM_DBDB,
      u_MM_PERM_DBDC,
      u_MM_PERM_DBDD,
      u_MM_PERM_DCAA,
      u_MM_PERM_DCAB,
      u_MM_PERM_DCAC,
      u_MM_PERM_DCAD,
      u_MM_PERM_DCBA,
      u_MM_PERM_DCBB,
      u_MM_PERM_DCBC,
      u_MM_PERM_DCBD,
      u_MM_PERM_DCCA,
      u_MM_PERM_DCCB,
      u_MM_PERM_DCCC,
      u_MM_PERM_DCCD,
      u_MM_PERM_DCDA,
      u_MM_PERM_DCDB,
      u_MM_PERM_DCDC,
      u_MM_PERM_DCDD,
      u_MM_PERM_DDAA,
      u_MM_PERM_DDAB,
      u_MM_PERM_DDAC,
      u_MM_PERM_DDAD,
      u_MM_PERM_DDBA,
      u_MM_PERM_DDBB,
      u_MM_PERM_DDBC,
      u_MM_PERM_DDBD,
      u_MM_PERM_DDCA,
      u_MM_PERM_DDCB,
      u_MM_PERM_DDCC,
      u_MM_PERM_DDCD,
      u_MM_PERM_DDDA,
      u_MM_PERM_DDDB,
      u_MM_PERM_DDDC,
      u_MM_PERM_DDDD);
   pragma Convention (C, u_MM_PERM_ENUM);  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avx512fintrin.h:3859

   --  skipped func _mm512_rolv_epi32

   --  skipped func _mm512_mask_rolv_epi32

   --  skipped func _mm512_maskz_rolv_epi32

   --  skipped func _mm512_rorv_epi32

   --  skipped func _mm512_mask_rorv_epi32

   --  skipped func _mm512_maskz_rorv_epi32

   --  skipped func _mm512_rolv_epi64

   --  skipped func _mm512_mask_rolv_epi64

   --  skipped func _mm512_maskz_rolv_epi64

   --  skipped func _mm512_rorv_epi64

   --  skipped func _mm512_mask_rorv_epi64

   --  skipped func _mm512_maskz_rorv_epi64

   --  skipped func _mm_cvtu32_sd

   --  skipped func _mm512_cvtepi32_epi8

   --  skipped func _mm512_mask_cvtepi32_storeu_epi8

   --  skipped func _mm512_mask_cvtepi32_epi8

   --  skipped func _mm512_maskz_cvtepi32_epi8

   --  skipped func _mm512_cvtsepi32_epi8

   --  skipped func _mm512_mask_cvtsepi32_storeu_epi8

   --  skipped func _mm512_mask_cvtsepi32_epi8

   --  skipped func _mm512_maskz_cvtsepi32_epi8

   --  skipped func _mm512_cvtusepi32_epi8

   --  skipped func _mm512_mask_cvtusepi32_storeu_epi8

   --  skipped func _mm512_mask_cvtusepi32_epi8

   --  skipped func _mm512_maskz_cvtusepi32_epi8

   --  skipped func _mm512_cvtepi32_epi16

   --  skipped func _mm512_mask_cvtepi32_storeu_epi16

   --  skipped func _mm512_mask_cvtepi32_epi16

   --  skipped func _mm512_maskz_cvtepi32_epi16

   --  skipped func _mm512_cvtsepi32_epi16

   --  skipped func _mm512_mask_cvtsepi32_storeu_epi16

   --  skipped func _mm512_mask_cvtsepi32_epi16

   --  skipped func _mm512_maskz_cvtsepi32_epi16

   --  skipped func _mm512_cvtusepi32_epi16

   --  skipped func _mm512_mask_cvtusepi32_storeu_epi16

   --  skipped func _mm512_mask_cvtusepi32_epi16

   --  skipped func _mm512_maskz_cvtusepi32_epi16

   --  skipped func _mm512_cvtepi64_epi32

   --  skipped func _mm512_mask_cvtepi64_storeu_epi32

   --  skipped func _mm512_mask_cvtepi64_epi32

   --  skipped func _mm512_maskz_cvtepi64_epi32

   --  skipped func _mm512_cvtsepi64_epi32

   --  skipped func _mm512_mask_cvtsepi64_storeu_epi32

   --  skipped func _mm512_mask_cvtsepi64_epi32

   --  skipped func _mm512_maskz_cvtsepi64_epi32

   --  skipped func _mm512_cvtusepi64_epi32

   --  skipped func _mm512_mask_cvtusepi64_storeu_epi32

   --  skipped func _mm512_mask_cvtusepi64_epi32

   --  skipped func _mm512_maskz_cvtusepi64_epi32

   --  skipped func _mm512_cvtepi64_epi16

   --  skipped func _mm512_mask_cvtepi64_storeu_epi16

   --  skipped func _mm512_mask_cvtepi64_epi16

   --  skipped func _mm512_maskz_cvtepi64_epi16

   --  skipped func _mm512_cvtsepi64_epi16

   --  skipped func _mm512_mask_cvtsepi64_storeu_epi16

   --  skipped func _mm512_mask_cvtsepi64_epi16

   --  skipped func _mm512_maskz_cvtsepi64_epi16

   --  skipped func _mm512_cvtusepi64_epi16

   --  skipped func _mm512_mask_cvtusepi64_storeu_epi16

   --  skipped func _mm512_mask_cvtusepi64_epi16

   --  skipped func _mm512_maskz_cvtusepi64_epi16

   --  skipped func _mm512_cvtepi64_epi8

   --  skipped func _mm512_mask_cvtepi64_storeu_epi8

   --  skipped func _mm512_mask_cvtepi64_epi8

   --  skipped func _mm512_maskz_cvtepi64_epi8

   --  skipped func _mm512_cvtsepi64_epi8

   --  skipped func _mm512_mask_cvtsepi64_storeu_epi8

   --  skipped func _mm512_mask_cvtsepi64_epi8

   --  skipped func _mm512_maskz_cvtsepi64_epi8

   --  skipped func _mm512_cvtusepi64_epi8

   --  skipped func _mm512_mask_cvtusepi64_storeu_epi8

   --  skipped func _mm512_mask_cvtusepi64_epi8

   --  skipped func _mm512_maskz_cvtusepi64_epi8

   --  skipped func _mm512_cvtepi32_pd

   --  skipped func _mm512_mask_cvtepi32_pd

   --  skipped func _mm512_maskz_cvtepi32_pd

   --  skipped func _mm512_cvtepu32_pd

   --  skipped func _mm512_mask_cvtepu32_pd

   --  skipped func _mm512_maskz_cvtepu32_pd

   --  skipped func _mm512_loadu_pd

   --  skipped func _mm512_mask_loadu_pd

   --  skipped func _mm512_maskz_loadu_pd

   --  skipped func _mm512_storeu_pd

   --  skipped func _mm512_mask_storeu_pd

   --  skipped func _mm512_loadu_ps

   --  skipped func _mm512_mask_loadu_ps

   --  skipped func _mm512_maskz_loadu_ps

   --  skipped func _mm512_storeu_ps

   --  skipped func _mm512_mask_storeu_ps

   --  skipped func _mm512_mask_loadu_epi64

   --  skipped func _mm512_maskz_loadu_epi64

   --  skipped func _mm512_mask_storeu_epi64

   --  skipped func _mm512_loadu_si512

   --  skipped func _mm512_mask_loadu_epi32

   --  skipped func _mm512_maskz_loadu_epi32

   --  skipped func _mm512_storeu_si512

   --  skipped func _mm512_mask_storeu_epi32

   --  skipped func _mm512_permutevar_pd

   --  skipped func _mm512_mask_permutevar_pd

   --  skipped func _mm512_maskz_permutevar_pd

   --  skipped func _mm512_permutevar_ps

   --  skipped func _mm512_mask_permutevar_ps

   --  skipped func _mm512_maskz_permutevar_ps

   --  skipped func _mm512_permutex2var_epi64

  -- idx  
   --  skipped func _mm512_mask_permutex2var_epi64

  -- idx  
   --  skipped func _mm512_mask2_permutex2var_epi64

  -- idx  
   --  skipped func _mm512_maskz_permutex2var_epi64

  -- idx  
   --  skipped func _mm512_permutex2var_epi32

  -- idx  
   --  skipped func _mm512_mask_permutex2var_epi32

  -- idx  
   --  skipped func _mm512_mask2_permutex2var_epi32

  -- idx  
   --  skipped func _mm512_maskz_permutex2var_epi32

  -- idx  
   --  skipped func _mm512_permutex2var_pd

  -- idx  
   --  skipped func _mm512_mask_permutex2var_pd

  -- idx  
   --  skipped func _mm512_mask2_permutex2var_pd

  -- idx  
   --  skipped func _mm512_maskz_permutex2var_pd

  -- idx  
   --  skipped func _mm512_permutex2var_ps

  -- idx  
   --  skipped func _mm512_mask_permutex2var_ps

  -- idx  
   --  skipped func _mm512_mask2_permutex2var_ps

  -- idx  
   --  skipped func _mm512_maskz_permutex2var_ps

  -- idx  
   --  skipped func _mm512_maskz_permutexvar_epi64

   --  skipped func _mm512_permutexvar_epi64

   --  skipped func _mm512_mask_permutexvar_epi64

   --  skipped func _mm512_maskz_permutexvar_epi32

   --  skipped func _mm512_permutexvar_epi32

   --  skipped func _mm512_mask_permutexvar_epi32

   --  skipped func _mm512_permutexvar_pd

   --  skipped func _mm512_mask_permutexvar_pd

   --  skipped func _mm512_maskz_permutexvar_pd

   --  skipped func _mm512_permutexvar_ps

   --  skipped func _mm512_mask_permutexvar_ps

   --  skipped func _mm512_maskz_permutexvar_ps

   --  skipped func _mm512_movehdup_ps

   --  skipped func _mm512_mask_movehdup_ps

   --  skipped func _mm512_maskz_movehdup_ps

   --  skipped func _mm512_moveldup_ps

   --  skipped func _mm512_mask_moveldup_ps

   --  skipped func _mm512_maskz_moveldup_ps

   --  skipped func _mm512_or_si512

   --  skipped func _mm512_or_epi32

   --  skipped func _mm512_mask_or_epi32

   --  skipped func _mm512_maskz_or_epi32

   --  skipped func _mm512_or_epi64

   --  skipped func _mm512_mask_or_epi64

   --  skipped func _mm512_maskz_or_epi64

   --  skipped func _mm512_xor_si512

   --  skipped func _mm512_xor_epi32

   --  skipped func _mm512_mask_xor_epi32

   --  skipped func _mm512_maskz_xor_epi32

   --  skipped func _mm512_xor_epi64

   --  skipped func _mm512_mask_xor_epi64

   --  skipped func _mm512_maskz_xor_epi64

   --  skipped func _mm512_and_si512

   --  skipped func _mm512_and_epi32

   --  skipped func _mm512_mask_and_epi32

   --  skipped func _mm512_maskz_and_epi32

   --  skipped func _mm512_and_epi64

   --  skipped func _mm512_mask_and_epi64

   --  skipped func _mm512_maskz_and_epi64

   --  skipped func _mm512_andnot_si512

   --  skipped func _mm512_andnot_epi32

   --  skipped func _mm512_mask_andnot_epi32

   --  skipped func _mm512_maskz_andnot_epi32

   --  skipped func _mm512_andnot_epi64

   --  skipped func _mm512_mask_andnot_epi64

   --  skipped func _mm512_maskz_andnot_epi64

   --  skipped func _mm512_test_epi32_mask

   --  skipped func _mm512_mask_test_epi32_mask

   --  skipped func _mm512_test_epi64_mask

   --  skipped func _mm512_mask_test_epi64_mask

   --  skipped func _mm512_testn_epi32_mask

   --  skipped func _mm512_mask_testn_epi32_mask

   --  skipped func _mm512_testn_epi64_mask

   --  skipped func _mm512_mask_testn_epi64_mask

   --  skipped func _mm512_abs_ps

   --  skipped func _mm512_mask_abs_ps

   --  skipped func _mm512_abs_pd

   --  skipped func _mm512_mask_abs_pd

   --  skipped func _mm512_unpackhi_epi32

   --  skipped func _mm512_mask_unpackhi_epi32

   --  skipped func _mm512_maskz_unpackhi_epi32

   --  skipped func _mm512_unpackhi_epi64

   --  skipped func _mm512_mask_unpackhi_epi64

   --  skipped func _mm512_maskz_unpackhi_epi64

   --  skipped func _mm512_unpacklo_epi32

   --  skipped func _mm512_mask_unpacklo_epi32

   --  skipped func _mm512_maskz_unpacklo_epi32

   --  skipped func _mm512_unpacklo_epi64

   --  skipped func _mm512_mask_unpacklo_epi64

   --  skipped func _mm512_maskz_unpacklo_epi64

   --  skipped func _mm512_movedup_pd

   --  skipped func _mm512_mask_movedup_pd

   --  skipped func _mm512_maskz_movedup_pd

   --  skipped func _mm512_unpacklo_pd

   --  skipped func _mm512_mask_unpacklo_pd

   --  skipped func _mm512_maskz_unpacklo_pd

   --  skipped func _mm512_unpackhi_pd

   --  skipped func _mm512_mask_unpackhi_pd

   --  skipped func _mm512_maskz_unpackhi_pd

   --  skipped func _mm512_unpackhi_ps

   --  skipped func _mm512_mask_unpackhi_ps

   --  skipped func _mm512_maskz_unpackhi_ps

   --  skipped func _mm512_stream_si512

   --  skipped func _mm512_stream_ps

   --  skipped func _mm512_stream_pd

   --  skipped func _mm512_stream_load_si512

  -- Constants for mantissa extraction  
  -- interval [1, 2)       
  -- interval [0.5, 2)     
  -- interval [0.5, 1)     
  -- interval [0.75, 1.5)  
   type u_MM_MANTISSA_NORM_ENUM is 
     (u_MM_MANT_NORM_1_2,
      u_MM_MANT_NORM_p5_2,
      u_MM_MANT_NORM_p5_1,
      u_MM_MANT_NORM_p75_1p5);
   pragma Convention (C, u_MM_MANTISSA_NORM_ENUM);  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avx512fintrin.h:8087

  -- sign = sign(SRC)      
  -- sign = 0              
  -- DEST = NaN if sign(SRC) = 1  
   type u_MM_MANTISSA_SIGN_ENUM is 
     (u_MM_MANT_SIGN_src,
      u_MM_MANT_SIGN_zero,
      u_MM_MANT_SIGN_nan);
   pragma Convention (C, u_MM_MANTISSA_SIGN_ENUM);  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avx512fintrin.h:8094

   --  skipped func _mm512_floor_ps

   --  skipped func _mm512_floor_pd

   --  skipped func _mm512_ceil_ps

   --  skipped func _mm512_ceil_pd

   --  skipped func _mm512_mask_floor_ps

   --  skipped func _mm512_mask_floor_pd

   --  skipped func _mm512_mask_ceil_ps

   --  skipped func _mm512_mask_ceil_pd

   --  skipped func _mm512_cmpeq_epi32_mask

   --  skipped func _mm512_mask_cmpeq_epi32_mask

   --  skipped func _mm512_mask_cmpeq_epi64_mask

   --  skipped func _mm512_cmpeq_epi64_mask

   --  skipped func _mm512_cmpgt_epi32_mask

   --  skipped func _mm512_mask_cmpgt_epi32_mask

   --  skipped func _mm512_mask_cmpgt_epi64_mask

   --  skipped func _mm512_cmpgt_epi64_mask

   --  skipped func _mm512_cmpge_epi32_mask

   --  skipped func _mm512_mask_cmpge_epi32_mask

   --  skipped func _mm512_mask_cmpge_epu32_mask

   --  skipped func _mm512_cmpge_epu32_mask

   --  skipped func _mm512_mask_cmpge_epi64_mask

   --  skipped func _mm512_cmpge_epi64_mask

   --  skipped func _mm512_mask_cmpge_epu64_mask

   --  skipped func _mm512_cmpge_epu64_mask

   --  skipped func _mm512_mask_cmple_epi32_mask

   --  skipped func _mm512_cmple_epi32_mask

   --  skipped func _mm512_mask_cmple_epu32_mask

   --  skipped func _mm512_cmple_epu32_mask

   --  skipped func _mm512_mask_cmple_epi64_mask

   --  skipped func _mm512_cmple_epi64_mask

   --  skipped func _mm512_mask_cmple_epu64_mask

   --  skipped func _mm512_cmple_epu64_mask

   --  skipped func _mm512_mask_cmplt_epi32_mask

   --  skipped func _mm512_cmplt_epi32_mask

   --  skipped func _mm512_mask_cmplt_epu32_mask

   --  skipped func _mm512_cmplt_epu32_mask

   --  skipped func _mm512_mask_cmplt_epi64_mask

   --  skipped func _mm512_cmplt_epi64_mask

   --  skipped func _mm512_mask_cmplt_epu64_mask

   --  skipped func _mm512_cmplt_epu64_mask

   --  skipped func _mm512_cmpneq_epi32_mask

   --  skipped func _mm512_mask_cmpneq_epi32_mask

   --  skipped func _mm512_mask_cmpneq_epu32_mask

   --  skipped func _mm512_cmpneq_epu32_mask

   --  skipped func _mm512_mask_cmpneq_epi64_mask

   --  skipped func _mm512_cmpneq_epi64_mask

   --  skipped func _mm512_mask_cmpneq_epu64_mask

   --  skipped func _mm512_cmpneq_epu64_mask

   --  skipped func _mm512_mask_compress_pd

   --  skipped func _mm512_maskz_compress_pd

   --  skipped func _mm512_mask_compressstoreu_pd

   --  skipped func _mm512_mask_compress_ps

   --  skipped func _mm512_maskz_compress_ps

   --  skipped func _mm512_mask_compressstoreu_ps

   --  skipped func _mm512_mask_compress_epi64

   --  skipped func _mm512_maskz_compress_epi64

   --  skipped func _mm512_mask_compressstoreu_epi64

   --  skipped func _mm512_mask_compress_epi32

   --  skipped func _mm512_maskz_compress_epi32

   --  skipped func _mm512_mask_compressstoreu_epi32

   --  skipped func _mm512_mask_expand_pd

   --  skipped func _mm512_maskz_expand_pd

   --  skipped func _mm512_mask_expandloadu_pd

   --  skipped func _mm512_maskz_expandloadu_pd

   --  skipped func _mm512_mask_expand_ps

   --  skipped func _mm512_maskz_expand_ps

   --  skipped func _mm512_mask_expandloadu_ps

   --  skipped func _mm512_maskz_expandloadu_ps

   --  skipped func _mm512_mask_expand_epi64

   --  skipped func _mm512_maskz_expand_epi64

   --  skipped func _mm512_mask_expandloadu_epi64

   --  skipped func _mm512_maskz_expandloadu_epi64

   --  skipped func _mm512_mask_expand_epi32

   --  skipped func _mm512_maskz_expand_epi32

   --  skipped func _mm512_mask_expandloadu_epi32

   --  skipped func _mm512_maskz_expandloadu_epi32

  -- Mask arithmetic operations  
   --  skipped func _kortest_mask16_u8

   --  skipped func _kortestz_mask16_u8

   --  skipped func _kortestc_mask16_u8

   --  skipped func _cvtmask16_u32

   --  skipped func _cvtu32_mask16

   --  skipped func _load_mask16

   --  skipped func _store_mask16

   --  skipped func _mm512_kand

   --  skipped func _mm512_kandn

   --  skipped func _mm512_kor

   --  skipped func _mm512_kortestz

   --  skipped func _mm512_kortestc

   --  skipped func _mm512_kxnor

   --  skipped func _mm512_kxor

   --  skipped func _mm512_knot

   --  skipped func _mm512_kunpackb

   --  skipped func _kunpackb_mask16

   --  skipped func _mm512_max_epi64

   --  skipped func _mm512_maskz_max_epi64

   --  skipped func _mm512_mask_max_epi64

   --  skipped func _mm512_min_epi64

   --  skipped func _mm512_mask_min_epi64

   --  skipped func _mm512_maskz_min_epi64

   --  skipped func _mm512_max_epu64

   --  skipped func _mm512_maskz_max_epu64

   --  skipped func _mm512_mask_max_epu64

   --  skipped func _mm512_min_epu64

   --  skipped func _mm512_mask_min_epu64

   --  skipped func _mm512_maskz_min_epu64

   --  skipped func _mm512_max_epi32

   --  skipped func _mm512_maskz_max_epi32

   --  skipped func _mm512_mask_max_epi32

   --  skipped func _mm512_min_epi32

   --  skipped func _mm512_maskz_min_epi32

   --  skipped func _mm512_mask_min_epi32

   --  skipped func _mm512_max_epu32

   --  skipped func _mm512_maskz_max_epu32

   --  skipped func _mm512_mask_max_epu32

   --  skipped func _mm512_min_epu32

   --  skipped func _mm512_maskz_min_epu32

   --  skipped func _mm512_mask_min_epu32

   --  skipped func _mm512_unpacklo_ps

   --  skipped func _mm512_mask_unpacklo_ps

   --  skipped func _mm512_maskz_unpacklo_ps

   --  skipped func _mm512_mask_blend_pd

   --  skipped func _mm512_mask_blend_ps

   --  skipped func _mm512_mask_blend_epi64

   --  skipped func _mm512_mask_blend_epi32

   --  skipped func _mm512_sqrt_pd

   --  skipped func _mm512_mask_sqrt_pd

   --  skipped func _mm512_maskz_sqrt_pd

   --  skipped func _mm512_sqrt_ps

   --  skipped func _mm512_mask_sqrt_ps

   --  skipped func _mm512_maskz_sqrt_ps

   --  skipped func _mm512_add_pd

   --  skipped func _mm512_mask_add_pd

   --  skipped func _mm512_maskz_add_pd

   --  skipped func _mm512_add_ps

   --  skipped func _mm512_mask_add_ps

   --  skipped func _mm512_maskz_add_ps

   --  skipped func _mm512_sub_pd

   --  skipped func _mm512_mask_sub_pd

   --  skipped func _mm512_maskz_sub_pd

   --  skipped func _mm512_sub_ps

   --  skipped func _mm512_mask_sub_ps

   --  skipped func _mm512_maskz_sub_ps

   --  skipped func _mm512_mul_pd

   --  skipped func _mm512_mask_mul_pd

   --  skipped func _mm512_maskz_mul_pd

   --  skipped func _mm512_mul_ps

   --  skipped func _mm512_mask_mul_ps

   --  skipped func _mm512_maskz_mul_ps

   --  skipped func _mm512_div_pd

   --  skipped func _mm512_mask_div_pd

   --  skipped func _mm512_maskz_div_pd

   --  skipped func _mm512_div_ps

   --  skipped func _mm512_mask_div_ps

   --  skipped func _mm512_maskz_div_ps

   --  skipped func _mm512_max_pd

   --  skipped func _mm512_mask_max_pd

   --  skipped func _mm512_maskz_max_pd

   --  skipped func _mm512_max_ps

   --  skipped func _mm512_mask_max_ps

   --  skipped func _mm512_maskz_max_ps

   --  skipped func _mm512_min_pd

   --  skipped func _mm512_mask_min_pd

   --  skipped func _mm512_maskz_min_pd

   --  skipped func _mm512_min_ps

   --  skipped func _mm512_mask_min_ps

   --  skipped func _mm512_maskz_min_ps

   --  skipped func _mm512_scalef_pd

   --  skipped func _mm512_mask_scalef_pd

   --  skipped func _mm512_maskz_scalef_pd

   --  skipped func _mm512_scalef_ps

   --  skipped func _mm512_mask_scalef_ps

   --  skipped func _mm512_maskz_scalef_ps

   --  skipped func _mm_scalef_sd

   --  skipped func _mm_scalef_ss

   --  skipped func _mm512_fmadd_pd

   --  skipped func _mm512_mask_fmadd_pd

   --  skipped func _mm512_mask3_fmadd_pd

   --  skipped func _mm512_maskz_fmadd_pd

   --  skipped func _mm512_fmadd_ps

   --  skipped func _mm512_mask_fmadd_ps

   --  skipped func _mm512_mask3_fmadd_ps

   --  skipped func _mm512_maskz_fmadd_ps

   --  skipped func _mm512_fmsub_pd

   --  skipped func _mm512_mask_fmsub_pd

   --  skipped func _mm512_mask3_fmsub_pd

   --  skipped func _mm512_maskz_fmsub_pd

   --  skipped func _mm512_fmsub_ps

   --  skipped func _mm512_mask_fmsub_ps

   --  skipped func _mm512_mask3_fmsub_ps

   --  skipped func _mm512_maskz_fmsub_ps

   --  skipped func _mm512_fmaddsub_pd

   --  skipped func _mm512_mask_fmaddsub_pd

   --  skipped func _mm512_mask3_fmaddsub_pd

   --  skipped func _mm512_maskz_fmaddsub_pd

   --  skipped func _mm512_fmaddsub_ps

   --  skipped func _mm512_mask_fmaddsub_ps

   --  skipped func _mm512_mask3_fmaddsub_ps

   --  skipped func _mm512_maskz_fmaddsub_ps

   --  skipped func _mm512_fmsubadd_pd

   --  skipped func _mm512_mask_fmsubadd_pd

   --  skipped func _mm512_mask3_fmsubadd_pd

   --  skipped func _mm512_maskz_fmsubadd_pd

   --  skipped func _mm512_fmsubadd_ps

   --  skipped func _mm512_mask_fmsubadd_ps

   --  skipped func _mm512_mask3_fmsubadd_ps

   --  skipped func _mm512_maskz_fmsubadd_ps

   --  skipped func _mm512_fnmadd_pd

   --  skipped func _mm512_mask_fnmadd_pd

   --  skipped func _mm512_mask3_fnmadd_pd

   --  skipped func _mm512_maskz_fnmadd_pd

   --  skipped func _mm512_fnmadd_ps

   --  skipped func _mm512_mask_fnmadd_ps

   --  skipped func _mm512_mask3_fnmadd_ps

   --  skipped func _mm512_maskz_fnmadd_ps

   --  skipped func _mm512_fnmsub_pd

   --  skipped func _mm512_mask_fnmsub_pd

   --  skipped func _mm512_mask3_fnmsub_pd

   --  skipped func _mm512_maskz_fnmsub_pd

   --  skipped func _mm512_fnmsub_ps

   --  skipped func _mm512_mask_fnmsub_ps

   --  skipped func _mm512_mask3_fnmsub_ps

   --  skipped func _mm512_maskz_fnmsub_ps

   --  skipped func _mm512_cvttpd_epi32

   --  skipped func _mm512_mask_cvttpd_epi32

   --  skipped func _mm512_maskz_cvttpd_epi32

   --  skipped func _mm512_cvttpd_epu32

   --  skipped func _mm512_mask_cvttpd_epu32

   --  skipped func _mm512_maskz_cvttpd_epu32

   --  skipped func _mm512_cvtpd_epi32

   --  skipped func _mm512_mask_cvtpd_epi32

   --  skipped func _mm512_maskz_cvtpd_epi32

   --  skipped func _mm512_cvtpd_epu32

   --  skipped func _mm512_mask_cvtpd_epu32

   --  skipped func _mm512_maskz_cvtpd_epu32

   --  skipped func _mm512_cvttps_epi32

   --  skipped func _mm512_mask_cvttps_epi32

   --  skipped func _mm512_maskz_cvttps_epi32

   --  skipped func _mm512_cvttps_epu32

   --  skipped func _mm512_mask_cvttps_epu32

   --  skipped func _mm512_maskz_cvttps_epu32

   --  skipped func _mm512_cvtps_epi32

   --  skipped func _mm512_mask_cvtps_epi32

   --  skipped func _mm512_maskz_cvtps_epi32

   --  skipped func _mm512_cvtps_epu32

   --  skipped func _mm512_mask_cvtps_epu32

   --  skipped func _mm512_maskz_cvtps_epu32

   --  skipped func _mm512_cvtsd_f64

   --  skipped func _mm512_cvtss_f32

   --  skipped func _mm_cvtu64_ss

   --  skipped func _mm_cvtu64_sd

   --  skipped func _mm_cvtu32_ss

   --  skipped func _mm512_cvtepi32_ps

   --  skipped func _mm512_mask_cvtepi32_ps

   --  skipped func _mm512_maskz_cvtepi32_ps

   --  skipped func _mm512_cvtepu32_ps

   --  skipped func _mm512_mask_cvtepu32_ps

   --  skipped func _mm512_maskz_cvtepu32_ps

   --  skipped func _mm_cvtss_u64

   --  skipped func _mm_cvttss_u64

   --  skipped func _mm_cvttss_i64

   --  skipped func _mm_cvtss_u32

   --  skipped func _mm_cvttss_u32

   --  skipped func _mm_cvttss_i32

   --  skipped func _mm_cvtsd_u64

   --  skipped func _mm_cvttsd_u64

   --  skipped func _mm_cvttsd_i64

   --  skipped func _mm_cvtsd_u32

   --  skipped func _mm_cvttsd_u32

   --  skipped func _mm_cvttsd_i32

   --  skipped func _mm512_cvtps_pd

   --  skipped func _mm512_mask_cvtps_pd

   --  skipped func _mm512_maskz_cvtps_pd

   --  skipped func _mm512_cvtph_ps

   --  skipped func _mm512_mask_cvtph_ps

   --  skipped func _mm512_maskz_cvtph_ps

   --  skipped func _mm512_cvtpd_ps

   --  skipped func _mm512_mask_cvtpd_ps

   --  skipped func _mm512_maskz_cvtpd_ps

   --  skipped func _mm512_kmov

   --  skipped func _mm512_castpd_ps

   --  skipped func _mm512_castpd_si512

   --  skipped func _mm512_castps_pd

   --  skipped func _mm512_castps_si512

   --  skipped func _mm512_castsi512_ps

   --  skipped func _mm512_castsi512_pd

   --  skipped func _mm512_castpd512_pd128

   --  skipped func _mm512_castps512_ps128

   --  skipped func _mm512_castsi512_si128

   --  skipped func _mm512_castpd512_pd256

   --  skipped func _mm512_castps512_ps256

   --  skipped func _mm512_castsi512_si256

   --  skipped func _mm512_castpd128_pd512

   --  skipped func _mm512_castps128_ps512

   --  skipped func _mm512_castsi128_si512

   --  skipped func _mm512_castpd256_pd512

   --  skipped func _mm512_castps256_ps512

   --  skipped func _mm512_castsi256_si512

   --  skipped func _mm512_cmpeq_epu32_mask

   --  skipped func _mm512_mask_cmpeq_epu32_mask

   --  skipped func _mm512_mask_cmpeq_epu64_mask

   --  skipped func _mm512_cmpeq_epu64_mask

   --  skipped func _mm512_cmpgt_epu32_mask

   --  skipped func _mm512_mask_cmpgt_epu32_mask

   --  skipped func _mm512_mask_cmpgt_epu64_mask

   --  skipped func _mm512_cmpgt_epu64_mask

   --  skipped func _mm512_reduce_add_epi32

   --  skipped func _mm512_reduce_mul_epi32

   --  skipped func _mm512_reduce_and_epi32

   --  skipped func _mm512_reduce_or_epi32

   --  skipped func _mm512_mask_reduce_add_epi32

   --  skipped func _mm512_mask_reduce_mul_epi32

   --  skipped func _mm512_mask_reduce_and_epi32

   --  skipped func _mm512_mask_reduce_or_epi32

   --  skipped func _mm512_reduce_min_epi32

   --  skipped func _mm512_reduce_max_epi32

   --  skipped func _mm512_reduce_min_epu32

   --  skipped func _mm512_reduce_max_epu32

   --  skipped func _mm512_mask_reduce_min_epi32

   --  skipped func _mm512_mask_reduce_max_epi32

   --  skipped func _mm512_mask_reduce_min_epu32

   --  skipped func _mm512_mask_reduce_max_epu32

   --  skipped func _mm512_reduce_add_ps

   --  skipped func _mm512_reduce_mul_ps

   --  skipped func _mm512_mask_reduce_add_ps

   --  skipped func _mm512_mask_reduce_mul_ps

   --  skipped func _mm512_reduce_min_ps

   --  skipped func _mm512_reduce_max_ps

   --  skipped func _mm512_mask_reduce_min_ps

   --  skipped func _mm512_mask_reduce_max_ps

   --  skipped func _mm512_reduce_add_epi64

   --  skipped func _mm512_reduce_mul_epi64

   --  skipped func _mm512_reduce_and_epi64

   --  skipped func _mm512_reduce_or_epi64

   --  skipped func _mm512_mask_reduce_add_epi64

   --  skipped func _mm512_mask_reduce_mul_epi64

   --  skipped func _mm512_mask_reduce_and_epi64

   --  skipped func _mm512_mask_reduce_or_epi64

   --  skipped func _mm512_reduce_min_epi64

   --  skipped func _mm512_reduce_max_epi64

   --  skipped func _mm512_mask_reduce_min_epi64

   --  skipped func _mm512_mask_reduce_max_epi64

   --  skipped func _mm512_reduce_min_epu64

   --  skipped func _mm512_reduce_max_epu64

   --  skipped func _mm512_mask_reduce_min_epu64

   --  skipped func _mm512_mask_reduce_max_epu64

   --  skipped func _mm512_reduce_add_pd

   --  skipped func _mm512_reduce_mul_pd

   --  skipped func _mm512_mask_reduce_add_pd

   --  skipped func _mm512_mask_reduce_mul_pd

   --  skipped func _mm512_reduce_min_pd

   --  skipped func _mm512_reduce_max_pd

   --  skipped func _mm512_mask_reduce_min_pd

   --  skipped func _mm512_mask_reduce_max_pd

end avx512fintrin_h;
