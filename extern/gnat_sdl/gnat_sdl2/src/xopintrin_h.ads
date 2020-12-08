pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package xopintrin_h is

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

  -- Integer multiply/add instructions.  
   --  skipped func _mm_maccs_epi16

   --  skipped func _mm_macc_epi16

   --  skipped func _mm_maccsd_epi16

   --  skipped func _mm_maccd_epi16

   --  skipped func _mm_maccs_epi32

   --  skipped func _mm_macc_epi32

   --  skipped func _mm_maccslo_epi32

   --  skipped func _mm_macclo_epi32

   --  skipped func _mm_maccshi_epi32

   --  skipped func _mm_macchi_epi32

   --  skipped func _mm_maddsd_epi16

   --  skipped func _mm_maddd_epi16

  -- Packed Integer Horizontal Add and Subtract  
   --  skipped func _mm_haddw_epi8

   --  skipped func _mm_haddd_epi8

   --  skipped func _mm_haddq_epi8

   --  skipped func _mm_haddd_epi16

   --  skipped func _mm_haddq_epi16

   --  skipped func _mm_haddq_epi32

   --  skipped func _mm_haddw_epu8

   --  skipped func _mm_haddd_epu8

   --  skipped func _mm_haddq_epu8

   --  skipped func _mm_haddd_epu16

   --  skipped func _mm_haddq_epu16

   --  skipped func _mm_haddq_epu32

   --  skipped func _mm_hsubw_epi8

   --  skipped func _mm_hsubd_epi16

   --  skipped func _mm_hsubq_epi32

  -- Vector conditional move and permute  
   --  skipped func _mm_cmov_si128

   --  skipped func _mm_perm_epi8

  -- Packed Integer Rotates and Shifts
  --   Rotates - Non-Immediate form  

   --  skipped func _mm_rot_epi8

   --  skipped func _mm_rot_epi16

   --  skipped func _mm_rot_epi32

   --  skipped func _mm_rot_epi64

  -- Rotates - Immediate form  
  -- Shifts  
   --  skipped func _mm_shl_epi8

   --  skipped func _mm_shl_epi16

   --  skipped func _mm_shl_epi32

   --  skipped func _mm_shl_epi64

   --  skipped func _mm_sha_epi8

   --  skipped func _mm_sha_epi16

   --  skipped func _mm_sha_epi32

   --  skipped func _mm_sha_epi64

  -- Compare and Predicate Generation
  --   pcom (integer, unsigned bytes)  

   --  skipped func _mm_comlt_epu8

   --  skipped func _mm_comle_epu8

   --  skipped func _mm_comgt_epu8

   --  skipped func _mm_comge_epu8

   --  skipped func _mm_comeq_epu8

   --  skipped func _mm_comneq_epu8

   --  skipped func _mm_comfalse_epu8

   --  skipped func _mm_comtrue_epu8

  --pcom (integer, unsigned words)  
   --  skipped func _mm_comlt_epu16

   --  skipped func _mm_comle_epu16

   --  skipped func _mm_comgt_epu16

   --  skipped func _mm_comge_epu16

   --  skipped func _mm_comeq_epu16

   --  skipped func _mm_comneq_epu16

   --  skipped func _mm_comfalse_epu16

   --  skipped func _mm_comtrue_epu16

  --pcom (integer, unsigned double words)  
   --  skipped func _mm_comlt_epu32

   --  skipped func _mm_comle_epu32

   --  skipped func _mm_comgt_epu32

   --  skipped func _mm_comge_epu32

   --  skipped func _mm_comeq_epu32

   --  skipped func _mm_comneq_epu32

   --  skipped func _mm_comfalse_epu32

   --  skipped func _mm_comtrue_epu32

  --pcom (integer, unsigned quad words)  
   --  skipped func _mm_comlt_epu64

   --  skipped func _mm_comle_epu64

   --  skipped func _mm_comgt_epu64

   --  skipped func _mm_comge_epu64

   --  skipped func _mm_comeq_epu64

   --  skipped func _mm_comneq_epu64

   --  skipped func _mm_comfalse_epu64

   --  skipped func _mm_comtrue_epu64

  --pcom (integer, signed bytes)  
   --  skipped func _mm_comlt_epi8

   --  skipped func _mm_comle_epi8

   --  skipped func _mm_comgt_epi8

   --  skipped func _mm_comge_epi8

   --  skipped func _mm_comeq_epi8

   --  skipped func _mm_comneq_epi8

   --  skipped func _mm_comfalse_epi8

   --  skipped func _mm_comtrue_epi8

  --pcom (integer, signed words)  
   --  skipped func _mm_comlt_epi16

   --  skipped func _mm_comle_epi16

   --  skipped func _mm_comgt_epi16

   --  skipped func _mm_comge_epi16

   --  skipped func _mm_comeq_epi16

   --  skipped func _mm_comneq_epi16

   --  skipped func _mm_comfalse_epi16

   --  skipped func _mm_comtrue_epi16

  --pcom (integer, signed double words)  
   --  skipped func _mm_comlt_epi32

   --  skipped func _mm_comle_epi32

   --  skipped func _mm_comgt_epi32

   --  skipped func _mm_comge_epi32

   --  skipped func _mm_comeq_epi32

   --  skipped func _mm_comneq_epi32

   --  skipped func _mm_comfalse_epi32

   --  skipped func _mm_comtrue_epi32

  --pcom (integer, signed quad words)  
   --  skipped func _mm_comlt_epi64

   --  skipped func _mm_comle_epi64

   --  skipped func _mm_comgt_epi64

   --  skipped func _mm_comge_epi64

   --  skipped func _mm_comeq_epi64

   --  skipped func _mm_comneq_epi64

   --  skipped func _mm_comfalse_epi64

   --  skipped func _mm_comtrue_epi64

  -- FRCZ  
   --  skipped func _mm_frcz_ps

   --  skipped func _mm_frcz_pd

   --  skipped func _mm_frcz_ss

   --  skipped func _mm_frcz_sd

   --  skipped func _mm256_frcz_ps

   --  skipped func _mm256_frcz_pd

  -- PERMIL2  
end xopintrin_h;
