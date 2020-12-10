pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;

package avx512bwintrin_h is

  -- Copyright (C) 2014-2017 Free Software Foundation, Inc.
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
   subtype uu_v32hi is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avx512bwintrin.h:38

   subtype uu_v64qi is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avx512bwintrin.h:39

   subtype uu_mmask64 is Extensions.unsigned_long_long;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avx512bwintrin.h:41

   --  skipped func _ktest_mask32_u8

   --  skipped func _ktest_mask64_u8

   --  skipped func _ktestz_mask32_u8

   --  skipped func _ktestz_mask64_u8

   --  skipped func _ktestc_mask32_u8

   --  skipped func _ktestc_mask64_u8

   --  skipped func _kortest_mask32_u8

   --  skipped func _kortest_mask64_u8

   --  skipped func _kortestz_mask32_u8

   --  skipped func _kortestz_mask64_u8

   --  skipped func _kortestc_mask32_u8

   --  skipped func _kortestc_mask64_u8

   --  skipped func _kadd_mask32

   --  skipped func _kadd_mask64

   --  skipped func _cvtmask32_u32

   --  skipped func _cvtmask64_u64

   --  skipped func _cvtu32_mask32

   --  skipped func _cvtu64_mask64

   --  skipped func _load_mask32

   --  skipped func _load_mask64

   --  skipped func _store_mask32

   --  skipped func _store_mask64

   --  skipped func _knot_mask32

   --  skipped func _knot_mask64

   --  skipped func _kor_mask32

   --  skipped func _kor_mask64

   --  skipped func _kxnor_mask32

   --  skipped func _kxnor_mask64

   --  skipped func _kxor_mask32

   --  skipped func _kxor_mask64

   --  skipped func _kand_mask32

   --  skipped func _kand_mask64

   --  skipped func _kandn_mask32

   --  skipped func _kandn_mask64

   --  skipped func _mm512_mask_mov_epi16

   --  skipped func _mm512_maskz_mov_epi16

   --  skipped func _mm512_mask_loadu_epi16

   --  skipped func _mm512_maskz_loadu_epi16

   --  skipped func _mm512_mask_storeu_epi16

   --  skipped func _mm512_mask_mov_epi8

   --  skipped func _mm512_maskz_mov_epi8

   --  skipped func _mm512_kunpackw

   --  skipped func _kunpackw_mask32

   --  skipped func _mm512_kunpackd

   --  skipped func _kunpackd_mask64

   --  skipped func _mm512_mask_loadu_epi8

   --  skipped func _mm512_maskz_loadu_epi8

   --  skipped func _mm512_mask_storeu_epi8

   --  skipped func _mm512_sad_epu8

   --  skipped func _mm512_cvtepi16_epi8

   --  skipped func _mm512_mask_cvtepi16_epi8

   --  skipped func _mm512_maskz_cvtepi16_epi8

   --  skipped func _mm512_cvtsepi16_epi8

   --  skipped func _mm512_mask_cvtsepi16_epi8

   --  skipped func _mm512_maskz_cvtsepi16_epi8

   --  skipped func _mm512_cvtusepi16_epi8

   --  skipped func _mm512_mask_cvtusepi16_epi8

   --  skipped func _mm512_maskz_cvtusepi16_epi8

   --  skipped func _mm512_broadcastb_epi8

   --  skipped func _mm512_mask_broadcastb_epi8

   --  skipped func _mm512_maskz_broadcastb_epi8

   --  skipped func _mm512_mask_set1_epi8

   --  skipped func _mm512_maskz_set1_epi8

   --  skipped func _mm512_broadcastw_epi16

   --  skipped func _mm512_mask_broadcastw_epi16

   --  skipped func _mm512_maskz_broadcastw_epi16

   --  skipped func _mm512_mask_set1_epi16

   --  skipped func _mm512_maskz_set1_epi16

   --  skipped func _mm512_mulhrs_epi16

   --  skipped func _mm512_mask_mulhrs_epi16

   --  skipped func _mm512_maskz_mulhrs_epi16

   --  skipped func _mm512_mulhi_epi16

   --  skipped func _mm512_mask_mulhi_epi16

   --  skipped func _mm512_maskz_mulhi_epi16

   --  skipped func _mm512_mulhi_epu16

   --  skipped func _mm512_mask_mulhi_epu16

   --  skipped func _mm512_maskz_mulhi_epu16

   --  skipped func _mm512_mullo_epi16

   --  skipped func _mm512_mask_mullo_epi16

   --  skipped func _mm512_maskz_mullo_epi16

   --  skipped func _mm512_cvtepi8_epi16

   --  skipped func _mm512_mask_cvtepi8_epi16

   --  skipped func _mm512_maskz_cvtepi8_epi16

   --  skipped func _mm512_cvtepu8_epi16

   --  skipped func _mm512_mask_cvtepu8_epi16

   --  skipped func _mm512_maskz_cvtepu8_epi16

   --  skipped func _mm512_permutexvar_epi16

   --  skipped func _mm512_maskz_permutexvar_epi16

   --  skipped func _mm512_mask_permutexvar_epi16

   --  skipped func _mm512_permutex2var_epi16

  -- idx  
   --  skipped func _mm512_mask_permutex2var_epi16

  -- idx  
   --  skipped func _mm512_mask2_permutex2var_epi16

  -- idx  
   --  skipped func _mm512_maskz_permutex2var_epi16

  -- idx  
   --  skipped func _mm512_avg_epu8

   --  skipped func _mm512_mask_avg_epu8

   --  skipped func _mm512_maskz_avg_epu8

   --  skipped func _mm512_add_epi8

   --  skipped func _mm512_mask_add_epi8

   --  skipped func _mm512_maskz_add_epi8

   --  skipped func _mm512_sub_epi8

   --  skipped func _mm512_mask_sub_epi8

   --  skipped func _mm512_maskz_sub_epi8

   --  skipped func _mm512_avg_epu16

   --  skipped func _mm512_mask_avg_epu16

   --  skipped func _mm512_maskz_avg_epu16

   --  skipped func _mm512_subs_epi8

   --  skipped func _mm512_mask_subs_epi8

   --  skipped func _mm512_maskz_subs_epi8

   --  skipped func _mm512_subs_epu8

   --  skipped func _mm512_mask_subs_epu8

   --  skipped func _mm512_maskz_subs_epu8

   --  skipped func _mm512_adds_epi8

   --  skipped func _mm512_mask_adds_epi8

   --  skipped func _mm512_maskz_adds_epi8

   --  skipped func _mm512_adds_epu8

   --  skipped func _mm512_mask_adds_epu8

   --  skipped func _mm512_maskz_adds_epu8

   --  skipped func _mm512_sub_epi16

   --  skipped func _mm512_mask_sub_epi16

   --  skipped func _mm512_maskz_sub_epi16

   --  skipped func _mm512_subs_epi16

   --  skipped func _mm512_mask_subs_epi16

   --  skipped func _mm512_maskz_subs_epi16

   --  skipped func _mm512_subs_epu16

   --  skipped func _mm512_mask_subs_epu16

   --  skipped func _mm512_maskz_subs_epu16

   --  skipped func _mm512_add_epi16

   --  skipped func _mm512_mask_add_epi16

   --  skipped func _mm512_maskz_add_epi16

   --  skipped func _mm512_adds_epi16

   --  skipped func _mm512_mask_adds_epi16

   --  skipped func _mm512_maskz_adds_epi16

   --  skipped func _mm512_adds_epu16

   --  skipped func _mm512_mask_adds_epu16

   --  skipped func _mm512_maskz_adds_epu16

   --  skipped func _mm512_srl_epi16

   --  skipped func _mm512_mask_srl_epi16

   --  skipped func _mm512_maskz_srl_epi16

   --  skipped func _mm512_packs_epi16

   --  skipped func _mm512_sll_epi16

   --  skipped func _mm512_mask_sll_epi16

   --  skipped func _mm512_maskz_sll_epi16

   --  skipped func _mm512_maddubs_epi16

   --  skipped func _mm512_mask_maddubs_epi16

   --  skipped func _mm512_maskz_maddubs_epi16

   --  skipped func _mm512_madd_epi16

   --  skipped func _mm512_mask_madd_epi16

   --  skipped func _mm512_maskz_madd_epi16

   --  skipped func _mm512_unpackhi_epi8

   --  skipped func _mm512_mask_unpackhi_epi8

   --  skipped func _mm512_maskz_unpackhi_epi8

   --  skipped func _mm512_unpackhi_epi16

   --  skipped func _mm512_mask_unpackhi_epi16

   --  skipped func _mm512_maskz_unpackhi_epi16

   --  skipped func _mm512_unpacklo_epi8

   --  skipped func _mm512_mask_unpacklo_epi8

   --  skipped func _mm512_maskz_unpacklo_epi8

   --  skipped func _mm512_unpacklo_epi16

   --  skipped func _mm512_mask_unpacklo_epi16

   --  skipped func _mm512_maskz_unpacklo_epi16

   --  skipped func _mm512_cmpeq_epu8_mask

   --  skipped func _mm512_cmpeq_epi8_mask

   --  skipped func _mm512_mask_cmpeq_epu8_mask

   --  skipped func _mm512_mask_cmpeq_epi8_mask

   --  skipped func _mm512_cmpeq_epu16_mask

   --  skipped func _mm512_cmpeq_epi16_mask

   --  skipped func _mm512_mask_cmpeq_epu16_mask

   --  skipped func _mm512_mask_cmpeq_epi16_mask

   --  skipped func _mm512_cmpgt_epu8_mask

   --  skipped func _mm512_cmpgt_epi8_mask

   --  skipped func _mm512_mask_cmpgt_epu8_mask

   --  skipped func _mm512_mask_cmpgt_epi8_mask

   --  skipped func _mm512_cmpgt_epu16_mask

   --  skipped func _mm512_cmpgt_epi16_mask

   --  skipped func _mm512_mask_cmpgt_epu16_mask

   --  skipped func _mm512_mask_cmpgt_epi16_mask

   --  skipped func _mm512_movepi8_mask

   --  skipped func _mm512_movepi16_mask

   --  skipped func _mm512_movm_epi8

   --  skipped func _mm512_movm_epi16

   --  skipped func _mm512_test_epi8_mask

   --  skipped func _mm512_mask_test_epi8_mask

   --  skipped func _mm512_test_epi16_mask

   --  skipped func _mm512_mask_test_epi16_mask

   --  skipped func _mm512_testn_epi8_mask

   --  skipped func _mm512_mask_testn_epi8_mask

   --  skipped func _mm512_testn_epi16_mask

   --  skipped func _mm512_mask_testn_epi16_mask

   --  skipped func _mm512_shuffle_epi8

   --  skipped func _mm512_mask_shuffle_epi8

   --  skipped func _mm512_maskz_shuffle_epi8

   --  skipped func _mm512_min_epu16

   --  skipped func _mm512_maskz_min_epu16

   --  skipped func _mm512_mask_min_epu16

   --  skipped func _mm512_min_epi16

   --  skipped func _mm512_maskz_min_epi16

   --  skipped func _mm512_mask_min_epi16

   --  skipped func _mm512_max_epu8

   --  skipped func _mm512_maskz_max_epu8

   --  skipped func _mm512_mask_max_epu8

   --  skipped func _mm512_max_epi8

   --  skipped func _mm512_maskz_max_epi8

   --  skipped func _mm512_mask_max_epi8

   --  skipped func _mm512_min_epu8

   --  skipped func _mm512_maskz_min_epu8

   --  skipped func _mm512_mask_min_epu8

   --  skipped func _mm512_min_epi8

   --  skipped func _mm512_maskz_min_epi8

   --  skipped func _mm512_mask_min_epi8

   --  skipped func _mm512_max_epi16

   --  skipped func _mm512_maskz_max_epi16

   --  skipped func _mm512_mask_max_epi16

   --  skipped func _mm512_max_epu16

   --  skipped func _mm512_maskz_max_epu16

   --  skipped func _mm512_mask_max_epu16

   --  skipped func _mm512_sra_epi16

   --  skipped func _mm512_mask_sra_epi16

   --  skipped func _mm512_maskz_sra_epi16

   --  skipped func _mm512_srav_epi16

   --  skipped func _mm512_mask_srav_epi16

   --  skipped func _mm512_maskz_srav_epi16

   --  skipped func _mm512_srlv_epi16

   --  skipped func _mm512_mask_srlv_epi16

   --  skipped func _mm512_maskz_srlv_epi16

   --  skipped func _mm512_sllv_epi16

   --  skipped func _mm512_mask_sllv_epi16

   --  skipped func _mm512_maskz_sllv_epi16

   --  skipped func _mm512_mask_packs_epi16

   --  skipped func _mm512_maskz_packs_epi16

   --  skipped func _mm512_packus_epi16

   --  skipped func _mm512_mask_packus_epi16

   --  skipped func _mm512_maskz_packus_epi16

   --  skipped func _mm512_abs_epi8

   --  skipped func _mm512_mask_abs_epi8

   --  skipped func _mm512_maskz_abs_epi8

   --  skipped func _mm512_abs_epi16

   --  skipped func _mm512_mask_abs_epi16

   --  skipped func _mm512_maskz_abs_epi16

   --  skipped func _mm512_mask_cmpneq_epu8_mask

   --  skipped func _mm512_mask_cmplt_epu8_mask

   --  skipped func _mm512_mask_cmpge_epu8_mask

   --  skipped func _mm512_mask_cmple_epu8_mask

   --  skipped func _mm512_mask_cmpneq_epu16_mask

   --  skipped func _mm512_mask_cmplt_epu16_mask

   --  skipped func _mm512_mask_cmpge_epu16_mask

   --  skipped func _mm512_mask_cmple_epu16_mask

   --  skipped func _mm512_mask_cmpneq_epi8_mask

   --  skipped func _mm512_mask_cmplt_epi8_mask

   --  skipped func _mm512_mask_cmpge_epi8_mask

   --  skipped func _mm512_mask_cmple_epi8_mask

   --  skipped func _mm512_mask_cmpneq_epi16_mask

   --  skipped func _mm512_mask_cmplt_epi16_mask

   --  skipped func _mm512_mask_cmpge_epi16_mask

   --  skipped func _mm512_mask_cmple_epi16_mask

   --  skipped func _mm512_cmpneq_epu8_mask

   --  skipped func _mm512_cmplt_epu8_mask

   --  skipped func _mm512_cmpge_epu8_mask

   --  skipped func _mm512_cmple_epu8_mask

   --  skipped func _mm512_cmpneq_epu16_mask

   --  skipped func _mm512_cmplt_epu16_mask

   --  skipped func _mm512_cmpge_epu16_mask

   --  skipped func _mm512_cmple_epu16_mask

   --  skipped func _mm512_cmpneq_epi8_mask

   --  skipped func _mm512_cmplt_epi8_mask

   --  skipped func _mm512_cmpge_epi8_mask

   --  skipped func _mm512_cmple_epi8_mask

   --  skipped func _mm512_cmpneq_epi16_mask

   --  skipped func _mm512_cmplt_epi16_mask

   --  skipped func _mm512_cmpge_epi16_mask

   --  skipped func _mm512_cmple_epi16_mask

   --  skipped func _mm512_packs_epi32

   --  skipped func _mm512_maskz_packs_epi32

   --  skipped func _mm512_mask_packs_epi32

   --  skipped func _mm512_packus_epi32

   --  skipped func _mm512_maskz_packus_epi32

   --  skipped func _mm512_mask_packus_epi32

end avx512bwintrin_h;
