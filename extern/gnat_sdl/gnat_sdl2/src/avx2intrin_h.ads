pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package avx2intrin_h is

  -- Copyright (C) 2011-2017 Free Software Foundation, Inc.
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

  -- Sum absolute 8-bit integer difference of adjacent groups of 4
  --   byte integers in the first 2 operands.  Starting offsets within
  --   operands are determined by the 3rd mask operand.   

   --  skipped func _mm256_abs_epi8

   --  skipped func _mm256_abs_epi16

   --  skipped func _mm256_abs_epi32

   --  skipped func _mm256_packs_epi32

   --  skipped func _mm256_packs_epi16

   --  skipped func _mm256_packus_epi32

   --  skipped func _mm256_packus_epi16

   --  skipped func _mm256_add_epi8

   --  skipped func _mm256_add_epi16

   --  skipped func _mm256_add_epi32

   --  skipped func _mm256_add_epi64

   --  skipped func _mm256_adds_epi8

   --  skipped func _mm256_adds_epi16

   --  skipped func _mm256_adds_epu8

   --  skipped func _mm256_adds_epu16

  -- In that case (__N*8) will be in vreg, and insn will not be matched.  
  -- Use define instead  
   --  skipped func _mm256_and_si256

   --  skipped func _mm256_andnot_si256

   --  skipped func _mm256_avg_epu8

   --  skipped func _mm256_avg_epu16

   --  skipped func _mm256_blendv_epi8

   --  skipped func _mm256_cmpeq_epi8

   --  skipped func _mm256_cmpeq_epi16

   --  skipped func _mm256_cmpeq_epi32

   --  skipped func _mm256_cmpeq_epi64

   --  skipped func _mm256_cmpgt_epi8

   --  skipped func _mm256_cmpgt_epi16

   --  skipped func _mm256_cmpgt_epi32

   --  skipped func _mm256_cmpgt_epi64

   --  skipped func _mm256_hadd_epi16

   --  skipped func _mm256_hadd_epi32

   --  skipped func _mm256_hadds_epi16

   --  skipped func _mm256_hsub_epi16

   --  skipped func _mm256_hsub_epi32

   --  skipped func _mm256_hsubs_epi16

   --  skipped func _mm256_maddubs_epi16

   --  skipped func _mm256_madd_epi16

   --  skipped func _mm256_max_epi8

   --  skipped func _mm256_max_epi16

   --  skipped func _mm256_max_epi32

   --  skipped func _mm256_max_epu8

   --  skipped func _mm256_max_epu16

   --  skipped func _mm256_max_epu32

   --  skipped func _mm256_min_epi8

   --  skipped func _mm256_min_epi16

   --  skipped func _mm256_min_epi32

   --  skipped func _mm256_min_epu8

   --  skipped func _mm256_min_epu16

   --  skipped func _mm256_min_epu32

   --  skipped func _mm256_movemask_epi8

   --  skipped func _mm256_cvtepi8_epi16

   --  skipped func _mm256_cvtepi8_epi32

   --  skipped func _mm256_cvtepi8_epi64

   --  skipped func _mm256_cvtepi16_epi32

   --  skipped func _mm256_cvtepi16_epi64

   --  skipped func _mm256_cvtepi32_epi64

   --  skipped func _mm256_cvtepu8_epi16

   --  skipped func _mm256_cvtepu8_epi32

   --  skipped func _mm256_cvtepu8_epi64

   --  skipped func _mm256_cvtepu16_epi32

   --  skipped func _mm256_cvtepu16_epi64

   --  skipped func _mm256_cvtepu32_epi64

   --  skipped func _mm256_mul_epi32

   --  skipped func _mm256_mulhrs_epi16

   --  skipped func _mm256_mulhi_epu16

   --  skipped func _mm256_mulhi_epi16

   --  skipped func _mm256_mullo_epi16

   --  skipped func _mm256_mullo_epi32

   --  skipped func _mm256_mul_epu32

   --  skipped func _mm256_or_si256

   --  skipped func _mm256_sad_epu8

   --  skipped func _mm256_shuffle_epi8

   --  skipped func _mm256_sign_epi8

   --  skipped func _mm256_sign_epi16

   --  skipped func _mm256_sign_epi32

   --  skipped func _mm256_slli_epi16

   --  skipped func _mm256_sll_epi16

   --  skipped func _mm256_slli_epi32

   --  skipped func _mm256_sll_epi32

   --  skipped func _mm256_slli_epi64

   --  skipped func _mm256_sll_epi64

   --  skipped func _mm256_srai_epi16

   --  skipped func _mm256_sra_epi16

   --  skipped func _mm256_srai_epi32

   --  skipped func _mm256_sra_epi32

   --  skipped func _mm256_srli_epi16

   --  skipped func _mm256_srl_epi16

   --  skipped func _mm256_srli_epi32

   --  skipped func _mm256_srl_epi32

   --  skipped func _mm256_srli_epi64

   --  skipped func _mm256_srl_epi64

   --  skipped func _mm256_sub_epi8

   --  skipped func _mm256_sub_epi16

   --  skipped func _mm256_sub_epi32

   --  skipped func _mm256_sub_epi64

   --  skipped func _mm256_subs_epi8

   --  skipped func _mm256_subs_epi16

   --  skipped func _mm256_subs_epu8

   --  skipped func _mm256_subs_epu16

   --  skipped func _mm256_unpackhi_epi8

   --  skipped func _mm256_unpackhi_epi16

   --  skipped func _mm256_unpackhi_epi32

   --  skipped func _mm256_unpackhi_epi64

   --  skipped func _mm256_unpacklo_epi8

   --  skipped func _mm256_unpacklo_epi16

   --  skipped func _mm256_unpacklo_epi32

   --  skipped func _mm256_unpacklo_epi64

   --  skipped func _mm256_xor_si256

   --  skipped func _mm256_stream_load_si256

   --  skipped func _mm_broadcastss_ps

   --  skipped func _mm256_broadcastss_ps

   --  skipped func _mm256_broadcastsd_pd

   --  skipped func _mm256_broadcastsi128_si256

   --  skipped func _mm256_broadcastb_epi8

   --  skipped func _mm256_broadcastw_epi16

   --  skipped func _mm256_broadcastd_epi32

   --  skipped func _mm256_broadcastq_epi64

   --  skipped func _mm_broadcastb_epi8

   --  skipped func _mm_broadcastw_epi16

   --  skipped func _mm_broadcastd_epi32

   --  skipped func _mm_broadcastq_epi64

   --  skipped func _mm256_permutevar8x32_epi32

   --  skipped func _mm256_permutevar8x32_ps

   --  skipped func _mm256_maskload_epi32

   --  skipped func _mm256_maskload_epi64

   --  skipped func _mm_maskload_epi32

   --  skipped func _mm_maskload_epi64

   --  skipped func _mm256_maskstore_epi32

   --  skipped func _mm256_maskstore_epi64

   --  skipped func _mm_maskstore_epi32

   --  skipped func _mm_maskstore_epi64

   --  skipped func _mm256_sllv_epi32

   --  skipped func _mm_sllv_epi32

   --  skipped func _mm256_sllv_epi64

   --  skipped func _mm_sllv_epi64

   --  skipped func _mm256_srav_epi32

   --  skipped func _mm_srav_epi32

   --  skipped func _mm256_srlv_epi32

   --  skipped func _mm_srlv_epi32

   --  skipped func _mm256_srlv_epi64

   --  skipped func _mm_srlv_epi64

end avx2intrin_h;
