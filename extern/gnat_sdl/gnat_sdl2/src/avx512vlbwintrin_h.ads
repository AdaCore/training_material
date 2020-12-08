pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package avx512vlbwintrin_h is

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

   --  skipped func _mm256_mask_mov_epi8

   --  skipped func _mm256_maskz_mov_epi8

   --  skipped func _mm_mask_mov_epi8

   --  skipped func _mm_maskz_mov_epi8

   --  skipped func _mm256_mask_storeu_epi8

   --  skipped func _mm_mask_storeu_epi8

   --  skipped func _mm256_mask_loadu_epi16

   --  skipped func _mm256_maskz_loadu_epi16

   --  skipped func _mm_mask_loadu_epi16

   --  skipped func _mm_maskz_loadu_epi16

   --  skipped func _mm256_mask_mov_epi16

   --  skipped func _mm256_maskz_mov_epi16

   --  skipped func _mm_mask_mov_epi16

   --  skipped func _mm_maskz_mov_epi16

   --  skipped func _mm256_mask_loadu_epi8

   --  skipped func _mm256_maskz_loadu_epi8

   --  skipped func _mm_mask_loadu_epi8

   --  skipped func _mm_maskz_loadu_epi8

   --  skipped func _mm256_cvtepi16_epi8

   --  skipped func _mm256_mask_cvtepi16_epi8

   --  skipped func _mm256_maskz_cvtepi16_epi8

   --  skipped func _mm_cvtsepi16_epi8

   --  skipped func _mm_mask_cvtsepi16_epi8

   --  skipped func _mm_maskz_cvtsepi16_epi8

   --  skipped func _mm256_cvtsepi16_epi8

   --  skipped func _mm256_mask_cvtsepi16_epi8

   --  skipped func _mm256_maskz_cvtsepi16_epi8

   --  skipped func _mm_cvtusepi16_epi8

   --  skipped func _mm_mask_cvtusepi16_epi8

   --  skipped func _mm_maskz_cvtusepi16_epi8

   --  skipped func _mm256_cvtusepi16_epi8

   --  skipped func _mm256_mask_cvtusepi16_epi8

   --  skipped func _mm256_maskz_cvtusepi16_epi8

   --  skipped func _mm256_mask_broadcastb_epi8

   --  skipped func _mm256_maskz_broadcastb_epi8

   --  skipped func _mm256_mask_set1_epi8

   --  skipped func _mm256_maskz_set1_epi8

   --  skipped func _mm_mask_broadcastb_epi8

   --  skipped func _mm_maskz_broadcastb_epi8

   --  skipped func _mm_mask_set1_epi8

   --  skipped func _mm_maskz_set1_epi8

   --  skipped func _mm256_mask_broadcastw_epi16

   --  skipped func _mm256_maskz_broadcastw_epi16

   --  skipped func _mm256_mask_set1_epi16

   --  skipped func _mm256_maskz_set1_epi16

   --  skipped func _mm_mask_broadcastw_epi16

   --  skipped func _mm_maskz_broadcastw_epi16

   --  skipped func _mm_mask_set1_epi16

   --  skipped func _mm_maskz_set1_epi16

   --  skipped func _mm256_permutexvar_epi16

   --  skipped func _mm256_maskz_permutexvar_epi16

   --  skipped func _mm256_mask_permutexvar_epi16

   --  skipped func _mm_permutexvar_epi16

   --  skipped func _mm_maskz_permutexvar_epi16

   --  skipped func _mm_mask_permutexvar_epi16

   --  skipped func _mm256_permutex2var_epi16

  -- idx  
   --  skipped func _mm256_mask_permutex2var_epi16

  -- idx  
   --  skipped func _mm256_mask2_permutex2var_epi16

  -- idx  
   --  skipped func _mm256_maskz_permutex2var_epi16

  -- idx  
   --  skipped func _mm_permutex2var_epi16

  -- idx  
   --  skipped func _mm_mask_permutex2var_epi16

  -- idx  
   --  skipped func _mm_mask2_permutex2var_epi16

  -- idx  
   --  skipped func _mm_maskz_permutex2var_epi16

  -- idx  
   --  skipped func _mm256_mask_maddubs_epi16

   --  skipped func _mm256_maskz_maddubs_epi16

   --  skipped func _mm_mask_maddubs_epi16

   --  skipped func _mm_maskz_maddubs_epi16

   --  skipped func _mm256_mask_madd_epi16

   --  skipped func _mm256_maskz_madd_epi16

   --  skipped func _mm_mask_madd_epi16

   --  skipped func _mm_maskz_madd_epi16

   --  skipped func _mm_movepi8_mask

   --  skipped func _mm256_movepi8_mask

   --  skipped func _mm_movepi16_mask

   --  skipped func _mm256_movepi16_mask

   --  skipped func _mm_movm_epi8

   --  skipped func _mm256_movm_epi8

   --  skipped func _mm_movm_epi16

   --  skipped func _mm256_movm_epi16

   --  skipped func _mm_test_epi8_mask

   --  skipped func _mm_mask_test_epi8_mask

   --  skipped func _mm256_test_epi8_mask

   --  skipped func _mm256_mask_test_epi8_mask

   --  skipped func _mm_test_epi16_mask

   --  skipped func _mm_mask_test_epi16_mask

   --  skipped func _mm256_test_epi16_mask

   --  skipped func _mm256_mask_test_epi16_mask

   --  skipped func _mm256_maskz_min_epu16

   --  skipped func _mm256_mask_min_epu16

   --  skipped func _mm_maskz_min_epu16

   --  skipped func _mm_mask_min_epu16

   --  skipped func _mm256_maskz_min_epi16

   --  skipped func _mm256_mask_min_epi16

   --  skipped func _mm256_maskz_max_epu8

   --  skipped func _mm256_mask_max_epu8

   --  skipped func _mm_maskz_max_epu8

   --  skipped func _mm_mask_max_epu8

   --  skipped func _mm256_maskz_max_epi8

   --  skipped func _mm256_mask_max_epi8

   --  skipped func _mm_maskz_max_epi8

   --  skipped func _mm_mask_max_epi8

   --  skipped func _mm256_maskz_min_epu8

   --  skipped func _mm256_mask_min_epu8

   --  skipped func _mm_maskz_min_epu8

   --  skipped func _mm_mask_min_epu8

   --  skipped func _mm256_maskz_min_epi8

   --  skipped func _mm256_mask_min_epi8

   --  skipped func _mm_maskz_min_epi8

   --  skipped func _mm_mask_min_epi8

   --  skipped func _mm256_maskz_max_epi16

   --  skipped func _mm256_mask_max_epi16

   --  skipped func _mm_maskz_max_epi16

   --  skipped func _mm_mask_max_epi16

   --  skipped func _mm256_maskz_max_epu16

   --  skipped func _mm256_mask_max_epu16

   --  skipped func _mm_maskz_max_epu16

   --  skipped func _mm_mask_max_epu16

   --  skipped func _mm_maskz_min_epi16

   --  skipped func _mm_mask_min_epi16

   --  skipped func _mm256_cmpneq_epi8_mask

   --  skipped func _mm256_cmplt_epi8_mask

   --  skipped func _mm256_cmpge_epi8_mask

   --  skipped func _mm256_cmple_epi8_mask

   --  skipped func _mm256_cmpneq_epi16_mask

   --  skipped func _mm256_cmplt_epi16_mask

   --  skipped func _mm256_cmpge_epi16_mask

   --  skipped func _mm256_cmple_epi16_mask

   --  skipped func _mm_cmpneq_epu8_mask

   --  skipped func _mm_cmplt_epu8_mask

   --  skipped func _mm_cmpge_epu8_mask

   --  skipped func _mm_cmple_epu8_mask

   --  skipped func _mm_cmpneq_epu16_mask

   --  skipped func _mm_cmplt_epu16_mask

   --  skipped func _mm_cmpge_epu16_mask

   --  skipped func _mm_cmple_epu16_mask

   --  skipped func _mm_cmpneq_epi8_mask

   --  skipped func _mm_cmplt_epi8_mask

   --  skipped func _mm_cmpge_epi8_mask

   --  skipped func _mm_cmple_epi8_mask

   --  skipped func _mm_cmpneq_epi16_mask

   --  skipped func _mm_cmplt_epi16_mask

   --  skipped func _mm_cmpge_epi16_mask

   --  skipped func _mm_cmple_epi16_mask

   --  skipped func _mm256_mask_mulhrs_epi16

   --  skipped func _mm256_maskz_mulhrs_epi16

   --  skipped func _mm256_mask_mulhi_epu16

   --  skipped func _mm256_maskz_mulhi_epu16

   --  skipped func _mm256_mask_mulhi_epi16

   --  skipped func _mm256_maskz_mulhi_epi16

   --  skipped func _mm_mask_mulhi_epi16

   --  skipped func _mm_maskz_mulhi_epi16

   --  skipped func _mm_mask_mulhi_epu16

   --  skipped func _mm_maskz_mulhi_epu16

   --  skipped func _mm_mask_mulhrs_epi16

   --  skipped func _mm_maskz_mulhrs_epi16

   --  skipped func _mm256_mask_mullo_epi16

   --  skipped func _mm256_maskz_mullo_epi16

   --  skipped func _mm_mask_mullo_epi16

   --  skipped func _mm_maskz_mullo_epi16

   --  skipped func _mm256_mask_cvtepi8_epi16

   --  skipped func _mm256_maskz_cvtepi8_epi16

   --  skipped func _mm_mask_cvtepi8_epi16

   --  skipped func _mm_maskz_cvtepi8_epi16

   --  skipped func _mm256_mask_cvtepu8_epi16

   --  skipped func _mm256_maskz_cvtepu8_epi16

   --  skipped func _mm_mask_cvtepu8_epi16

   --  skipped func _mm_maskz_cvtepu8_epi16

   --  skipped func _mm256_mask_avg_epu8

   --  skipped func _mm256_maskz_avg_epu8

   --  skipped func _mm_mask_avg_epu8

   --  skipped func _mm_maskz_avg_epu8

   --  skipped func _mm256_mask_avg_epu16

   --  skipped func _mm256_maskz_avg_epu16

   --  skipped func _mm_mask_avg_epu16

   --  skipped func _mm_maskz_avg_epu16

   --  skipped func _mm256_mask_add_epi8

   --  skipped func _mm256_maskz_add_epi8

   --  skipped func _mm256_mask_add_epi16

   --  skipped func _mm256_maskz_add_epi16

   --  skipped func _mm256_mask_adds_epi8

   --  skipped func _mm256_maskz_adds_epi8

   --  skipped func _mm256_mask_adds_epi16

   --  skipped func _mm256_maskz_adds_epi16

   --  skipped func _mm256_mask_adds_epu8

   --  skipped func _mm256_maskz_adds_epu8

   --  skipped func _mm256_mask_adds_epu16

   --  skipped func _mm256_maskz_adds_epu16

   --  skipped func _mm256_mask_sub_epi8

   --  skipped func _mm256_maskz_sub_epi8

   --  skipped func _mm256_mask_sub_epi16

   --  skipped func _mm256_maskz_sub_epi16

   --  skipped func _mm256_mask_subs_epi8

   --  skipped func _mm256_maskz_subs_epi8

   --  skipped func _mm256_mask_subs_epi16

   --  skipped func _mm256_maskz_subs_epi16

   --  skipped func _mm256_mask_subs_epu8

   --  skipped func _mm256_maskz_subs_epu8

   --  skipped func _mm256_mask_subs_epu16

   --  skipped func _mm256_maskz_subs_epu16

   --  skipped func _mm_mask_add_epi8

   --  skipped func _mm_maskz_add_epi8

   --  skipped func _mm_mask_add_epi16

   --  skipped func _mm_maskz_add_epi16

   --  skipped func _mm256_mask_unpackhi_epi8

   --  skipped func _mm256_maskz_unpackhi_epi8

   --  skipped func _mm_mask_unpackhi_epi8

   --  skipped func _mm_maskz_unpackhi_epi8

   --  skipped func _mm256_mask_unpackhi_epi16

   --  skipped func _mm256_maskz_unpackhi_epi16

   --  skipped func _mm_mask_unpackhi_epi16

   --  skipped func _mm_maskz_unpackhi_epi16

   --  skipped func _mm256_mask_unpacklo_epi8

   --  skipped func _mm256_maskz_unpacklo_epi8

   --  skipped func _mm_mask_unpacklo_epi8

   --  skipped func _mm_maskz_unpacklo_epi8

   --  skipped func _mm256_mask_unpacklo_epi16

   --  skipped func _mm256_maskz_unpacklo_epi16

   --  skipped func _mm_mask_unpacklo_epi16

   --  skipped func _mm_maskz_unpacklo_epi16

   --  skipped func _mm_cmpeq_epi8_mask

   --  skipped func _mm_cmpeq_epu8_mask

   --  skipped func _mm_mask_cmpeq_epu8_mask

   --  skipped func _mm_mask_cmpeq_epi8_mask

   --  skipped func _mm256_cmpeq_epu8_mask

   --  skipped func _mm256_cmpeq_epi8_mask

   --  skipped func _mm256_mask_cmpeq_epu8_mask

   --  skipped func _mm256_mask_cmpeq_epi8_mask

   --  skipped func _mm_cmpeq_epu16_mask

   --  skipped func _mm_cmpeq_epi16_mask

   --  skipped func _mm_mask_cmpeq_epu16_mask

   --  skipped func _mm_mask_cmpeq_epi16_mask

   --  skipped func _mm256_cmpeq_epu16_mask

   --  skipped func _mm256_cmpeq_epi16_mask

   --  skipped func _mm256_mask_cmpeq_epu16_mask

   --  skipped func _mm256_mask_cmpeq_epi16_mask

   --  skipped func _mm_cmpgt_epu8_mask

   --  skipped func _mm_cmpgt_epi8_mask

   --  skipped func _mm_mask_cmpgt_epu8_mask

   --  skipped func _mm_mask_cmpgt_epi8_mask

   --  skipped func _mm256_cmpgt_epu8_mask

   --  skipped func _mm256_cmpgt_epi8_mask

   --  skipped func _mm256_mask_cmpgt_epu8_mask

   --  skipped func _mm256_mask_cmpgt_epi8_mask

   --  skipped func _mm_cmpgt_epu16_mask

   --  skipped func _mm_cmpgt_epi16_mask

   --  skipped func _mm_mask_cmpgt_epu16_mask

   --  skipped func _mm_mask_cmpgt_epi16_mask

   --  skipped func _mm256_cmpgt_epu16_mask

   --  skipped func _mm256_cmpgt_epi16_mask

   --  skipped func _mm256_mask_cmpgt_epu16_mask

   --  skipped func _mm256_mask_cmpgt_epi16_mask

   --  skipped func _mm_testn_epi8_mask

   --  skipped func _mm_mask_testn_epi8_mask

   --  skipped func _mm256_testn_epi8_mask

   --  skipped func _mm256_mask_testn_epi8_mask

   --  skipped func _mm_testn_epi16_mask

   --  skipped func _mm_mask_testn_epi16_mask

   --  skipped func _mm256_testn_epi16_mask

   --  skipped func _mm256_mask_testn_epi16_mask

   --  skipped func _mm256_mask_shuffle_epi8

   --  skipped func _mm256_maskz_shuffle_epi8

   --  skipped func _mm_mask_shuffle_epi8

   --  skipped func _mm_maskz_shuffle_epi8

   --  skipped func _mm256_maskz_packs_epi16

   --  skipped func _mm256_mask_packs_epi16

   --  skipped func _mm_maskz_packs_epi16

   --  skipped func _mm_mask_packs_epi16

   --  skipped func _mm256_maskz_packus_epi16

   --  skipped func _mm256_mask_packus_epi16

   --  skipped func _mm_maskz_packus_epi16

   --  skipped func _mm_mask_packus_epi16

   --  skipped func _mm256_mask_abs_epi8

   --  skipped func _mm256_maskz_abs_epi8

   --  skipped func _mm_mask_abs_epi8

   --  skipped func _mm_maskz_abs_epi8

   --  skipped func _mm256_mask_abs_epi16

   --  skipped func _mm256_maskz_abs_epi16

   --  skipped func _mm_mask_abs_epi16

   --  skipped func _mm_maskz_abs_epi16

   --  skipped func _mm256_cmpneq_epu8_mask

   --  skipped func _mm256_cmplt_epu8_mask

   --  skipped func _mm256_cmpge_epu8_mask

   --  skipped func _mm256_cmple_epu8_mask

   --  skipped func _mm256_cmpneq_epu16_mask

   --  skipped func _mm256_cmplt_epu16_mask

   --  skipped func _mm256_cmpge_epu16_mask

   --  skipped func _mm256_cmple_epu16_mask

   --  skipped func _mm256_mask_storeu_epi16

   --  skipped func _mm_mask_storeu_epi16

   --  skipped func _mm_mask_adds_epi16

   --  skipped func _mm_mask_subs_epi8

   --  skipped func _mm_maskz_subs_epi8

   --  skipped func _mm_mask_subs_epi16

   --  skipped func _mm_maskz_subs_epi16

   --  skipped func _mm_mask_subs_epu8

   --  skipped func _mm_maskz_subs_epu8

   --  skipped func _mm_mask_subs_epu16

   --  skipped func _mm_maskz_subs_epu16

   --  skipped func _mm256_mask_srl_epi16

   --  skipped func _mm256_maskz_srl_epi16

   --  skipped func _mm_mask_srl_epi16

   --  skipped func _mm_maskz_srl_epi16

   --  skipped func _mm256_mask_sra_epi16

   --  skipped func _mm256_maskz_sra_epi16

   --  skipped func _mm_mask_sra_epi16

   --  skipped func _mm_maskz_sra_epi16

   --  skipped func _mm_maskz_adds_epi16

   --  skipped func _mm_mask_adds_epu8

   --  skipped func _mm_maskz_adds_epu8

   --  skipped func _mm_mask_adds_epu16

   --  skipped func _mm_maskz_adds_epu16

   --  skipped func _mm_mask_sub_epi8

   --  skipped func _mm_maskz_sub_epi8

   --  skipped func _mm_mask_sub_epi16

   --  skipped func _mm_maskz_sub_epi16

   --  skipped func _mm_mask_adds_epi8

   --  skipped func _mm_maskz_adds_epi8

   --  skipped func _mm_cvtepi16_epi8

   --  skipped func _mm_mask_cvtepi16_epi8

   --  skipped func _mm_maskz_cvtepi16_epi8

   --  skipped func _mm256_srav_epi16

   --  skipped func _mm256_mask_srav_epi16

   --  skipped func _mm256_maskz_srav_epi16

   --  skipped func _mm_srav_epi16

   --  skipped func _mm_mask_srav_epi16

   --  skipped func _mm_maskz_srav_epi16

   --  skipped func _mm256_srlv_epi16

   --  skipped func _mm256_mask_srlv_epi16

   --  skipped func _mm256_maskz_srlv_epi16

   --  skipped func _mm_srlv_epi16

   --  skipped func _mm_mask_srlv_epi16

   --  skipped func _mm_maskz_srlv_epi16

   --  skipped func _mm256_sllv_epi16

   --  skipped func _mm256_mask_sllv_epi16

   --  skipped func _mm256_maskz_sllv_epi16

   --  skipped func _mm_sllv_epi16

   --  skipped func _mm_mask_sllv_epi16

   --  skipped func _mm_maskz_sllv_epi16

   --  skipped func _mm_mask_sll_epi16

   --  skipped func _mm_maskz_sll_epi16

   --  skipped func _mm256_mask_sll_epi16

   --  skipped func _mm256_maskz_sll_epi16

   --  skipped func _mm256_maskz_packus_epi32

   --  skipped func _mm256_mask_packus_epi32

   --  skipped func _mm_maskz_packus_epi32

   --  skipped func _mm_mask_packus_epi32

   --  skipped func _mm256_maskz_packs_epi32

   --  skipped func _mm256_mask_packs_epi32

   --  skipped func _mm_maskz_packs_epi32

   --  skipped func _mm_mask_packs_epi32

   --  skipped func _mm_mask_cmpneq_epu8_mask

   --  skipped func _mm_mask_cmplt_epu8_mask

   --  skipped func _mm_mask_cmpge_epu8_mask

   --  skipped func _mm_mask_cmple_epu8_mask

   --  skipped func _mm_mask_cmpneq_epu16_mask

   --  skipped func _mm_mask_cmplt_epu16_mask

   --  skipped func _mm_mask_cmpge_epu16_mask

   --  skipped func _mm_mask_cmple_epu16_mask

   --  skipped func _mm_mask_cmpneq_epi8_mask

   --  skipped func _mm_mask_cmplt_epi8_mask

   --  skipped func _mm_mask_cmpge_epi8_mask

   --  skipped func _mm_mask_cmple_epi8_mask

   --  skipped func _mm_mask_cmpneq_epi16_mask

   --  skipped func _mm_mask_cmplt_epi16_mask

   --  skipped func _mm_mask_cmpge_epi16_mask

   --  skipped func _mm_mask_cmple_epi16_mask

   --  skipped func _mm256_mask_cmpneq_epu8_mask

   --  skipped func _mm256_mask_cmplt_epu8_mask

   --  skipped func _mm256_mask_cmpge_epu8_mask

   --  skipped func _mm256_mask_cmple_epu8_mask

   --  skipped func _mm256_mask_cmpneq_epu16_mask

   --  skipped func _mm256_mask_cmplt_epu16_mask

   --  skipped func _mm256_mask_cmpge_epu16_mask

   --  skipped func _mm256_mask_cmple_epu16_mask

   --  skipped func _mm256_mask_cmpneq_epi8_mask

   --  skipped func _mm256_mask_cmplt_epi8_mask

   --  skipped func _mm256_mask_cmpge_epi8_mask

   --  skipped func _mm256_mask_cmple_epi8_mask

   --  skipped func _mm256_mask_cmpneq_epi16_mask

   --  skipped func _mm256_mask_cmplt_epi16_mask

   --  skipped func _mm256_mask_cmpge_epi16_mask

   --  skipped func _mm256_mask_cmple_epi16_mask

end avx512vlbwintrin_h;
