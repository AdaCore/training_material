pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package avx512vlintrin_h is

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
   subtype uu_mmask32 is unsigned;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avx512vlintrin.h:38

   --  skipped func _mm256_mask_mov_pd

   --  skipped func _mm256_maskz_mov_pd

   --  skipped func _mm_mask_mov_pd

   --  skipped func _mm_maskz_mov_pd

   --  skipped func _mm256_mask_load_pd

   --  skipped func _mm256_maskz_load_pd

   --  skipped func _mm_mask_load_pd

   --  skipped func _mm_maskz_load_pd

   --  skipped func _mm256_mask_store_pd

   --  skipped func _mm_mask_store_pd

   --  skipped func _mm256_mask_mov_ps

   --  skipped func _mm256_maskz_mov_ps

   --  skipped func _mm_mask_mov_ps

   --  skipped func _mm_maskz_mov_ps

   --  skipped func _mm256_mask_load_ps

   --  skipped func _mm256_maskz_load_ps

   --  skipped func _mm_mask_load_ps

   --  skipped func _mm_maskz_load_ps

   --  skipped func _mm256_mask_store_ps

   --  skipped func _mm_mask_store_ps

   --  skipped func _mm256_mask_mov_epi64

   --  skipped func _mm256_maskz_mov_epi64

   --  skipped func _mm_mask_mov_epi64

   --  skipped func _mm_maskz_mov_epi64

   --  skipped func _mm256_mask_load_epi64

   --  skipped func _mm256_maskz_load_epi64

   --  skipped func _mm_mask_load_epi64

   --  skipped func _mm_maskz_load_epi64

   --  skipped func _mm256_mask_store_epi64

   --  skipped func _mm_mask_store_epi64

   --  skipped func _mm256_mask_mov_epi32

   --  skipped func _mm256_maskz_mov_epi32

   --  skipped func _mm_mask_mov_epi32

   --  skipped func _mm_maskz_mov_epi32

   --  skipped func _mm256_mask_load_epi32

   --  skipped func _mm256_maskz_load_epi32

   --  skipped func _mm_mask_load_epi32

   --  skipped func _mm_maskz_load_epi32

   --  skipped func _mm256_mask_store_epi32

   --  skipped func _mm_mask_store_epi32

   --  skipped func _mm_mask_add_pd

   --  skipped func _mm_maskz_add_pd

   --  skipped func _mm256_mask_add_pd

   --  skipped func _mm256_maskz_add_pd

   --  skipped func _mm_mask_add_ps

   --  skipped func _mm_maskz_add_ps

   --  skipped func _mm256_mask_add_ps

   --  skipped func _mm256_maskz_add_ps

   --  skipped func _mm_mask_sub_pd

   --  skipped func _mm_maskz_sub_pd

   --  skipped func _mm256_mask_sub_pd

   --  skipped func _mm256_maskz_sub_pd

   --  skipped func _mm_mask_sub_ps

   --  skipped func _mm_maskz_sub_ps

   --  skipped func _mm256_mask_sub_ps

   --  skipped func _mm256_maskz_sub_ps

   --  skipped func _mm256_store_epi64

   --  skipped func _mm_store_epi64

   --  skipped func _mm256_mask_loadu_pd

   --  skipped func _mm256_maskz_loadu_pd

   --  skipped func _mm_mask_loadu_pd

   --  skipped func _mm_maskz_loadu_pd

   --  skipped func _mm256_mask_storeu_pd

   --  skipped func _mm_mask_storeu_pd

   --  skipped func _mm256_mask_loadu_ps

   --  skipped func _mm256_maskz_loadu_ps

   --  skipped func _mm_mask_loadu_ps

   --  skipped func _mm_maskz_loadu_ps

   --  skipped func _mm256_mask_storeu_ps

   --  skipped func _mm_mask_storeu_ps

   --  skipped func _mm256_mask_loadu_epi64

   --  skipped func _mm256_maskz_loadu_epi64

   --  skipped func _mm_mask_loadu_epi64

   --  skipped func _mm_maskz_loadu_epi64

   --  skipped func _mm256_mask_storeu_epi64

   --  skipped func _mm_mask_storeu_epi64

   --  skipped func _mm256_mask_loadu_epi32

   --  skipped func _mm256_maskz_loadu_epi32

   --  skipped func _mm_mask_loadu_epi32

   --  skipped func _mm_maskz_loadu_epi32

   --  skipped func _mm256_mask_storeu_epi32

   --  skipped func _mm_mask_storeu_epi32

   --  skipped func _mm256_mask_abs_epi32

   --  skipped func _mm256_maskz_abs_epi32

   --  skipped func _mm_mask_abs_epi32

   --  skipped func _mm_maskz_abs_epi32

   --  skipped func _mm256_abs_epi64

   --  skipped func _mm256_mask_abs_epi64

   --  skipped func _mm256_maskz_abs_epi64

   --  skipped func _mm_abs_epi64

   --  skipped func _mm_mask_abs_epi64

   --  skipped func _mm_maskz_abs_epi64

   --  skipped func _mm256_cvtpd_epu32

   --  skipped func _mm256_mask_cvtpd_epu32

   --  skipped func _mm256_maskz_cvtpd_epu32

   --  skipped func _mm_cvtpd_epu32

   --  skipped func _mm_mask_cvtpd_epu32

   --  skipped func _mm_maskz_cvtpd_epu32

   --  skipped func _mm256_mask_cvttps_epi32

   --  skipped func _mm256_maskz_cvttps_epi32

   --  skipped func _mm_mask_cvttps_epi32

   --  skipped func _mm_maskz_cvttps_epi32

   --  skipped func _mm256_cvttps_epu32

   --  skipped func _mm256_mask_cvttps_epu32

   --  skipped func _mm256_maskz_cvttps_epu32

   --  skipped func _mm_cvttps_epu32

   --  skipped func _mm_mask_cvttps_epu32

   --  skipped func _mm_maskz_cvttps_epu32

   --  skipped func _mm256_mask_cvttpd_epi32

   --  skipped func _mm256_maskz_cvttpd_epi32

   --  skipped func _mm_mask_cvttpd_epi32

   --  skipped func _mm_maskz_cvttpd_epi32

   --  skipped func _mm256_cvttpd_epu32

   --  skipped func _mm256_mask_cvttpd_epu32

   --  skipped func _mm256_maskz_cvttpd_epu32

   --  skipped func _mm_cvttpd_epu32

   --  skipped func _mm_mask_cvttpd_epu32

   --  skipped func _mm_maskz_cvttpd_epu32

   --  skipped func _mm256_mask_cvtpd_epi32

   --  skipped func _mm256_maskz_cvtpd_epi32

   --  skipped func _mm_mask_cvtpd_epi32

   --  skipped func _mm_maskz_cvtpd_epi32

   --  skipped func _mm256_mask_cvtepi32_pd

   --  skipped func _mm256_maskz_cvtepi32_pd

   --  skipped func _mm_mask_cvtepi32_pd

   --  skipped func _mm_maskz_cvtepi32_pd

   --  skipped func _mm256_cvtepu32_pd

   --  skipped func _mm256_mask_cvtepu32_pd

   --  skipped func _mm256_maskz_cvtepu32_pd

   --  skipped func _mm_cvtepu32_pd

   --  skipped func _mm_mask_cvtepu32_pd

   --  skipped func _mm_maskz_cvtepu32_pd

   --  skipped func _mm256_mask_cvtepi32_ps

   --  skipped func _mm256_maskz_cvtepi32_ps

   --  skipped func _mm_mask_cvtepi32_ps

   --  skipped func _mm_maskz_cvtepi32_ps

   --  skipped func _mm256_cvtepu32_ps

   --  skipped func _mm256_mask_cvtepu32_ps

   --  skipped func _mm256_maskz_cvtepu32_ps

   --  skipped func _mm_cvtepu32_ps

   --  skipped func _mm_mask_cvtepu32_ps

   --  skipped func _mm_maskz_cvtepu32_ps

   --  skipped func _mm256_mask_cvtps_pd

   --  skipped func _mm256_maskz_cvtps_pd

   --  skipped func _mm_mask_cvtps_pd

   --  skipped func _mm_maskz_cvtps_pd

   --  skipped func _mm_cvtepi32_epi8

   --  skipped func _mm_mask_cvtepi32_storeu_epi8

   --  skipped func _mm_mask_cvtepi32_epi8

   --  skipped func _mm_maskz_cvtepi32_epi8

   --  skipped func _mm256_cvtepi32_epi8

   --  skipped func _mm256_mask_cvtepi32_epi8

   --  skipped func _mm256_mask_cvtepi32_storeu_epi8

   --  skipped func _mm256_maskz_cvtepi32_epi8

   --  skipped func _mm_cvtsepi32_epi8

   --  skipped func _mm_mask_cvtsepi32_storeu_epi8

   --  skipped func _mm_mask_cvtsepi32_epi8

   --  skipped func _mm_maskz_cvtsepi32_epi8

   --  skipped func _mm256_cvtsepi32_epi8

   --  skipped func _mm256_mask_cvtsepi32_storeu_epi8

   --  skipped func _mm256_mask_cvtsepi32_epi8

   --  skipped func _mm256_maskz_cvtsepi32_epi8

   --  skipped func _mm_cvtusepi32_epi8

   --  skipped func _mm_mask_cvtusepi32_storeu_epi8

   --  skipped func _mm_mask_cvtusepi32_epi8

   --  skipped func _mm_maskz_cvtusepi32_epi8

   --  skipped func _mm256_cvtusepi32_epi8

   --  skipped func _mm256_mask_cvtusepi32_storeu_epi8

   --  skipped func _mm256_mask_cvtusepi32_epi8

   --  skipped func _mm256_maskz_cvtusepi32_epi8

   --  skipped func _mm_cvtepi32_epi16

   --  skipped func _mm_mask_cvtepi32_storeu_epi16

   --  skipped func _mm_mask_cvtepi32_epi16

   --  skipped func _mm_maskz_cvtepi32_epi16

   --  skipped func _mm256_cvtepi32_epi16

   --  skipped func _mm256_mask_cvtepi32_storeu_epi16

   --  skipped func _mm256_mask_cvtepi32_epi16

   --  skipped func _mm256_maskz_cvtepi32_epi16

   --  skipped func _mm_cvtsepi32_epi16

   --  skipped func _mm_mask_cvtsepi32_storeu_epi16

   --  skipped func _mm_mask_cvtsepi32_epi16

   --  skipped func _mm_maskz_cvtsepi32_epi16

   --  skipped func _mm256_cvtsepi32_epi16

   --  skipped func _mm256_mask_cvtsepi32_storeu_epi16

   --  skipped func _mm256_mask_cvtsepi32_epi16

   --  skipped func _mm256_maskz_cvtsepi32_epi16

   --  skipped func _mm_cvtusepi32_epi16

   --  skipped func _mm_mask_cvtusepi32_storeu_epi16

   --  skipped func _mm_mask_cvtusepi32_epi16

   --  skipped func _mm_maskz_cvtusepi32_epi16

   --  skipped func _mm256_cvtusepi32_epi16

   --  skipped func _mm256_mask_cvtusepi32_storeu_epi16

   --  skipped func _mm256_mask_cvtusepi32_epi16

   --  skipped func _mm256_maskz_cvtusepi32_epi16

   --  skipped func _mm_cvtepi64_epi8

   --  skipped func _mm_mask_cvtepi64_storeu_epi8

   --  skipped func _mm_mask_cvtepi64_epi8

   --  skipped func _mm_maskz_cvtepi64_epi8

   --  skipped func _mm256_cvtepi64_epi8

   --  skipped func _mm256_mask_cvtepi64_storeu_epi8

   --  skipped func _mm256_mask_cvtepi64_epi8

   --  skipped func _mm256_maskz_cvtepi64_epi8

   --  skipped func _mm_cvtsepi64_epi8

   --  skipped func _mm_mask_cvtsepi64_storeu_epi8

   --  skipped func _mm_mask_cvtsepi64_epi8

   --  skipped func _mm_maskz_cvtsepi64_epi8

   --  skipped func _mm256_cvtsepi64_epi8

   --  skipped func _mm256_mask_cvtsepi64_storeu_epi8

   --  skipped func _mm256_mask_cvtsepi64_epi8

   --  skipped func _mm256_maskz_cvtsepi64_epi8

   --  skipped func _mm_cvtusepi64_epi8

   --  skipped func _mm_mask_cvtusepi64_storeu_epi8

   --  skipped func _mm_mask_cvtusepi64_epi8

   --  skipped func _mm_maskz_cvtusepi64_epi8

   --  skipped func _mm256_cvtusepi64_epi8

   --  skipped func _mm256_mask_cvtusepi64_storeu_epi8

   --  skipped func _mm256_mask_cvtusepi64_epi8

   --  skipped func _mm256_maskz_cvtusepi64_epi8

   --  skipped func _mm_cvtepi64_epi16

   --  skipped func _mm_mask_cvtepi64_storeu_epi16

   --  skipped func _mm_mask_cvtepi64_epi16

   --  skipped func _mm_maskz_cvtepi64_epi16

   --  skipped func _mm256_cvtepi64_epi16

   --  skipped func _mm256_mask_cvtepi64_storeu_epi16

   --  skipped func _mm256_mask_cvtepi64_epi16

   --  skipped func _mm256_maskz_cvtepi64_epi16

   --  skipped func _mm_cvtsepi64_epi16

   --  skipped func _mm_mask_cvtsepi64_storeu_epi16

   --  skipped func _mm_mask_cvtsepi64_epi16

   --  skipped func _mm_maskz_cvtsepi64_epi16

   --  skipped func _mm256_cvtsepi64_epi16

   --  skipped func _mm256_mask_cvtsepi64_storeu_epi16

   --  skipped func _mm256_mask_cvtsepi64_epi16

   --  skipped func _mm256_maskz_cvtsepi64_epi16

   --  skipped func _mm_cvtusepi64_epi16

   --  skipped func _mm_mask_cvtusepi64_storeu_epi16

   --  skipped func _mm_mask_cvtusepi64_epi16

   --  skipped func _mm_maskz_cvtusepi64_epi16

   --  skipped func _mm256_cvtusepi64_epi16

   --  skipped func _mm256_mask_cvtusepi64_storeu_epi16

   --  skipped func _mm256_mask_cvtusepi64_epi16

   --  skipped func _mm256_maskz_cvtusepi64_epi16

   --  skipped func _mm_cvtepi64_epi32

   --  skipped func _mm_mask_cvtepi64_storeu_epi32

   --  skipped func _mm_mask_cvtepi64_epi32

   --  skipped func _mm_maskz_cvtepi64_epi32

   --  skipped func _mm256_cvtepi64_epi32

   --  skipped func _mm256_mask_cvtepi64_storeu_epi32

   --  skipped func _mm256_mask_cvtepi64_epi32

   --  skipped func _mm256_maskz_cvtepi64_epi32

   --  skipped func _mm_cvtsepi64_epi32

   --  skipped func _mm_mask_cvtsepi64_storeu_epi32

   --  skipped func _mm_mask_cvtsepi64_epi32

   --  skipped func _mm_maskz_cvtsepi64_epi32

   --  skipped func _mm256_cvtsepi64_epi32

   --  skipped func _mm256_mask_cvtsepi64_storeu_epi32

   --  skipped func _mm256_mask_cvtsepi64_epi32

   --  skipped func _mm256_maskz_cvtsepi64_epi32

   --  skipped func _mm_cvtusepi64_epi32

   --  skipped func _mm_mask_cvtusepi64_storeu_epi32

   --  skipped func _mm_mask_cvtusepi64_epi32

   --  skipped func _mm_maskz_cvtusepi64_epi32

   --  skipped func _mm256_cvtusepi64_epi32

   --  skipped func _mm256_mask_cvtusepi64_storeu_epi32

   --  skipped func _mm256_mask_cvtusepi64_epi32

   --  skipped func _mm256_maskz_cvtusepi64_epi32

   --  skipped func _mm256_mask_broadcastss_ps

   --  skipped func _mm256_maskz_broadcastss_ps

   --  skipped func _mm_mask_broadcastss_ps

   --  skipped func _mm_maskz_broadcastss_ps

   --  skipped func _mm256_mask_broadcastsd_pd

   --  skipped func _mm256_maskz_broadcastsd_pd

   --  skipped func _mm256_mask_broadcastd_epi32

   --  skipped func _mm256_maskz_broadcastd_epi32

   --  skipped func _mm256_mask_set1_epi32

   --  skipped func _mm256_maskz_set1_epi32

   --  skipped func _mm_mask_broadcastd_epi32

   --  skipped func _mm_maskz_broadcastd_epi32

   --  skipped func _mm_mask_set1_epi32

   --  skipped func _mm_maskz_set1_epi32

   --  skipped func _mm256_mask_broadcastq_epi64

   --  skipped func _mm256_maskz_broadcastq_epi64

   --  skipped func _mm256_mask_set1_epi64

   --  skipped func _mm256_maskz_set1_epi64

   --  skipped func _mm_mask_broadcastq_epi64

   --  skipped func _mm_maskz_broadcastq_epi64

   --  skipped func _mm_mask_set1_epi64

   --  skipped func _mm_maskz_set1_epi64

   --  skipped func _mm256_broadcast_f32x4

   --  skipped func _mm256_mask_broadcast_f32x4

   --  skipped func _mm256_maskz_broadcast_f32x4

   --  skipped func _mm256_broadcast_i32x4

   --  skipped func _mm256_mask_broadcast_i32x4

   --  skipped func _mm256_maskz_broadcast_i32x4

   --  skipped func _mm256_mask_cvtepi8_epi32

   --  skipped func _mm256_maskz_cvtepi8_epi32

   --  skipped func _mm_mask_cvtepi8_epi32

   --  skipped func _mm_maskz_cvtepi8_epi32

   --  skipped func _mm256_mask_cvtepi8_epi64

   --  skipped func _mm256_maskz_cvtepi8_epi64

   --  skipped func _mm_mask_cvtepi8_epi64

   --  skipped func _mm_maskz_cvtepi8_epi64

   --  skipped func _mm256_mask_cvtepi16_epi32

   --  skipped func _mm256_maskz_cvtepi16_epi32

   --  skipped func _mm_mask_cvtepi16_epi32

   --  skipped func _mm_maskz_cvtepi16_epi32

   --  skipped func _mm256_mask_cvtepi16_epi64

   --  skipped func _mm256_maskz_cvtepi16_epi64

   --  skipped func _mm_mask_cvtepi16_epi64

   --  skipped func _mm_maskz_cvtepi16_epi64

   --  skipped func _mm256_mask_cvtepi32_epi64

   --  skipped func _mm256_maskz_cvtepi32_epi64

   --  skipped func _mm_mask_cvtepi32_epi64

   --  skipped func _mm_maskz_cvtepi32_epi64

   --  skipped func _mm256_mask_cvtepu8_epi32

   --  skipped func _mm256_maskz_cvtepu8_epi32

   --  skipped func _mm_mask_cvtepu8_epi32

   --  skipped func _mm_maskz_cvtepu8_epi32

   --  skipped func _mm256_mask_cvtepu8_epi64

   --  skipped func _mm256_maskz_cvtepu8_epi64

   --  skipped func _mm_mask_cvtepu8_epi64

   --  skipped func _mm_maskz_cvtepu8_epi64

   --  skipped func _mm256_mask_cvtepu16_epi32

   --  skipped func _mm256_maskz_cvtepu16_epi32

   --  skipped func _mm_mask_cvtepu16_epi32

   --  skipped func _mm_maskz_cvtepu16_epi32

   --  skipped func _mm256_mask_cvtepu16_epi64

   --  skipped func _mm256_maskz_cvtepu16_epi64

   --  skipped func _mm_mask_cvtepu16_epi64

   --  skipped func _mm_maskz_cvtepu16_epi64

   --  skipped func _mm256_mask_cvtepu32_epi64

   --  skipped func _mm256_maskz_cvtepu32_epi64

   --  skipped func _mm_mask_cvtepu32_epi64

   --  skipped func _mm_maskz_cvtepu32_epi64

   --  skipped func _mm256_rcp14_pd

   --  skipped func _mm256_mask_rcp14_pd

   --  skipped func _mm256_maskz_rcp14_pd

   --  skipped func _mm_rcp14_pd

   --  skipped func _mm_mask_rcp14_pd

   --  skipped func _mm_maskz_rcp14_pd

   --  skipped func _mm256_rcp14_ps

   --  skipped func _mm256_mask_rcp14_ps

   --  skipped func _mm256_maskz_rcp14_ps

   --  skipped func _mm_rcp14_ps

   --  skipped func _mm_mask_rcp14_ps

   --  skipped func _mm_maskz_rcp14_ps

   --  skipped func _mm256_rsqrt14_pd

   --  skipped func _mm256_mask_rsqrt14_pd

   --  skipped func _mm256_maskz_rsqrt14_pd

   --  skipped func _mm_rsqrt14_pd

   --  skipped func _mm_mask_rsqrt14_pd

   --  skipped func _mm_maskz_rsqrt14_pd

   --  skipped func _mm256_rsqrt14_ps

   --  skipped func _mm256_mask_rsqrt14_ps

   --  skipped func _mm256_maskz_rsqrt14_ps

   --  skipped func _mm_rsqrt14_ps

   --  skipped func _mm_mask_rsqrt14_ps

   --  skipped func _mm_maskz_rsqrt14_ps

   --  skipped func _mm256_mask_sqrt_pd

   --  skipped func _mm256_maskz_sqrt_pd

   --  skipped func _mm_mask_sqrt_pd

   --  skipped func _mm_maskz_sqrt_pd

   --  skipped func _mm256_mask_sqrt_ps

   --  skipped func _mm256_maskz_sqrt_ps

   --  skipped func _mm_mask_sqrt_ps

   --  skipped func _mm_maskz_sqrt_ps

   --  skipped func _mm256_mask_add_epi32

   --  skipped func _mm256_maskz_add_epi32

   --  skipped func _mm256_mask_add_epi64

   --  skipped func _mm256_maskz_add_epi64

   --  skipped func _mm256_mask_sub_epi32

   --  skipped func _mm256_maskz_sub_epi32

   --  skipped func _mm256_mask_sub_epi64

   --  skipped func _mm256_maskz_sub_epi64

   --  skipped func _mm_mask_add_epi32

   --  skipped func _mm_maskz_add_epi32

   --  skipped func _mm_mask_add_epi64

   --  skipped func _mm_maskz_add_epi64

   --  skipped func _mm_mask_sub_epi32

   --  skipped func _mm_maskz_sub_epi32

   --  skipped func _mm_mask_sub_epi64

   --  skipped func _mm_maskz_sub_epi64

   --  skipped func _mm256_getexp_ps

   --  skipped func _mm256_mask_getexp_ps

   --  skipped func _mm256_maskz_getexp_ps

   --  skipped func _mm256_getexp_pd

   --  skipped func _mm256_mask_getexp_pd

   --  skipped func _mm256_maskz_getexp_pd

   --  skipped func _mm_getexp_ps

   --  skipped func _mm_mask_getexp_ps

   --  skipped func _mm_maskz_getexp_ps

   --  skipped func _mm_getexp_pd

   --  skipped func _mm_mask_getexp_pd

   --  skipped func _mm_maskz_getexp_pd

   --  skipped func _mm256_mask_srl_epi32

   --  skipped func _mm256_maskz_srl_epi32

   --  skipped func _mm_mask_srl_epi32

   --  skipped func _mm_maskz_srl_epi32

   --  skipped func _mm256_mask_srl_epi64

   --  skipped func _mm256_maskz_srl_epi64

   --  skipped func _mm_mask_srl_epi64

   --  skipped func _mm_maskz_srl_epi64

   --  skipped func _mm256_mask_and_epi32

   --  skipped func _mm256_maskz_and_epi32

   --  skipped func _mm256_scalef_pd

   --  skipped func _mm256_mask_scalef_pd

   --  skipped func _mm256_maskz_scalef_pd

   --  skipped func _mm256_scalef_ps

   --  skipped func _mm256_mask_scalef_ps

   --  skipped func _mm256_maskz_scalef_ps

   --  skipped func _mm_scalef_pd

   --  skipped func _mm_mask_scalef_pd

   --  skipped func _mm_maskz_scalef_pd

   --  skipped func _mm_scalef_ps

   --  skipped func _mm_mask_scalef_ps

   --  skipped func _mm_maskz_scalef_ps

   --  skipped func _mm256_mask_fmadd_pd

   --  skipped func _mm256_mask3_fmadd_pd

   --  skipped func _mm256_maskz_fmadd_pd

   --  skipped func _mm_mask_fmadd_pd

   --  skipped func _mm_mask3_fmadd_pd

   --  skipped func _mm_maskz_fmadd_pd

   --  skipped func _mm256_mask_fmadd_ps

   --  skipped func _mm256_mask3_fmadd_ps

   --  skipped func _mm256_maskz_fmadd_ps

   --  skipped func _mm_mask_fmadd_ps

   --  skipped func _mm_mask3_fmadd_ps

   --  skipped func _mm_maskz_fmadd_ps

   --  skipped func _mm256_mask_fmsub_pd

   --  skipped func _mm256_mask3_fmsub_pd

   --  skipped func _mm256_maskz_fmsub_pd

   --  skipped func _mm_mask_fmsub_pd

   --  skipped func _mm_mask3_fmsub_pd

   --  skipped func _mm_maskz_fmsub_pd

   --  skipped func _mm256_mask_fmsub_ps

   --  skipped func _mm256_mask3_fmsub_ps

   --  skipped func _mm256_maskz_fmsub_ps

   --  skipped func _mm_mask_fmsub_ps

   --  skipped func _mm_mask3_fmsub_ps

   --  skipped func _mm_maskz_fmsub_ps

   --  skipped func _mm256_mask_fmaddsub_pd

   --  skipped func _mm256_mask3_fmaddsub_pd

   --  skipped func _mm256_maskz_fmaddsub_pd

   --  skipped func _mm_mask_fmaddsub_pd

   --  skipped func _mm_mask3_fmaddsub_pd

   --  skipped func _mm_maskz_fmaddsub_pd

   --  skipped func _mm256_mask_fmaddsub_ps

   --  skipped func _mm256_mask3_fmaddsub_ps

   --  skipped func _mm256_maskz_fmaddsub_ps

   --  skipped func _mm_mask_fmaddsub_ps

   --  skipped func _mm_mask3_fmaddsub_ps

   --  skipped func _mm_maskz_fmaddsub_ps

   --  skipped func _mm256_mask_fmsubadd_pd

   --  skipped func _mm256_mask3_fmsubadd_pd

   --  skipped func _mm256_maskz_fmsubadd_pd

   --  skipped func _mm_mask_fmsubadd_pd

   --  skipped func _mm_mask3_fmsubadd_pd

   --  skipped func _mm_maskz_fmsubadd_pd

   --  skipped func _mm256_mask_fmsubadd_ps

   --  skipped func _mm256_mask3_fmsubadd_ps

   --  skipped func _mm256_maskz_fmsubadd_ps

   --  skipped func _mm_mask_fmsubadd_ps

   --  skipped func _mm_mask3_fmsubadd_ps

   --  skipped func _mm_maskz_fmsubadd_ps

   --  skipped func _mm256_mask_fnmadd_pd

   --  skipped func _mm256_mask3_fnmadd_pd

   --  skipped func _mm256_maskz_fnmadd_pd

   --  skipped func _mm_mask_fnmadd_pd

   --  skipped func _mm_mask3_fnmadd_pd

   --  skipped func _mm_maskz_fnmadd_pd

   --  skipped func _mm256_mask_fnmadd_ps

   --  skipped func _mm256_mask3_fnmadd_ps

   --  skipped func _mm256_maskz_fnmadd_ps

   --  skipped func _mm_mask_fnmadd_ps

   --  skipped func _mm_mask3_fnmadd_ps

   --  skipped func _mm_maskz_fnmadd_ps

   --  skipped func _mm256_mask_fnmsub_pd

   --  skipped func _mm256_mask3_fnmsub_pd

   --  skipped func _mm256_maskz_fnmsub_pd

   --  skipped func _mm_mask_fnmsub_pd

   --  skipped func _mm_mask3_fnmsub_pd

   --  skipped func _mm_maskz_fnmsub_pd

   --  skipped func _mm256_mask_fnmsub_ps

   --  skipped func _mm256_mask3_fnmsub_ps

   --  skipped func _mm256_maskz_fnmsub_ps

   --  skipped func _mm_mask_fnmsub_ps

   --  skipped func _mm_mask3_fnmsub_ps

   --  skipped func _mm_maskz_fnmsub_ps

   --  skipped func _mm_mask_and_epi32

   --  skipped func _mm_maskz_and_epi32

   --  skipped func _mm256_mask_andnot_epi32

   --  skipped func _mm256_maskz_andnot_epi32

   --  skipped func _mm_mask_andnot_epi32

   --  skipped func _mm_maskz_andnot_epi32

   --  skipped func _mm256_mask_or_epi32

   --  skipped func _mm256_maskz_or_epi32

   --  skipped func _mm_mask_or_epi32

   --  skipped func _mm_maskz_or_epi32

   --  skipped func _mm256_mask_xor_epi32

   --  skipped func _mm256_maskz_xor_epi32

   --  skipped func _mm_mask_xor_epi32

   --  skipped func _mm_maskz_xor_epi32

   --  skipped func _mm_mask_cvtpd_ps

   --  skipped func _mm_maskz_cvtpd_ps

   --  skipped func _mm256_mask_cvtpd_ps

   --  skipped func _mm256_maskz_cvtpd_ps

   --  skipped func _mm256_mask_cvtps_epi32

   --  skipped func _mm256_maskz_cvtps_epi32

   --  skipped func _mm_mask_cvtps_epi32

   --  skipped func _mm_maskz_cvtps_epi32

   --  skipped func _mm256_cvtps_epu32

   --  skipped func _mm256_mask_cvtps_epu32

   --  skipped func _mm256_maskz_cvtps_epu32

   --  skipped func _mm_cvtps_epu32

   --  skipped func _mm_mask_cvtps_epu32

   --  skipped func _mm_maskz_cvtps_epu32

   --  skipped func _mm256_mask_movedup_pd

   --  skipped func _mm256_maskz_movedup_pd

   --  skipped func _mm_mask_movedup_pd

   --  skipped func _mm_maskz_movedup_pd

   --  skipped func _mm256_mask_movehdup_ps

   --  skipped func _mm256_maskz_movehdup_ps

   --  skipped func _mm_mask_movehdup_ps

   --  skipped func _mm_maskz_movehdup_ps

   --  skipped func _mm256_mask_moveldup_ps

   --  skipped func _mm256_maskz_moveldup_ps

   --  skipped func _mm_mask_moveldup_ps

   --  skipped func _mm_maskz_moveldup_ps

   --  skipped func _mm_mask_unpackhi_epi32

   --  skipped func _mm_maskz_unpackhi_epi32

   --  skipped func _mm256_mask_unpackhi_epi32

   --  skipped func _mm256_maskz_unpackhi_epi32

   --  skipped func _mm_mask_unpackhi_epi64

   --  skipped func _mm_maskz_unpackhi_epi64

   --  skipped func _mm256_mask_unpackhi_epi64

   --  skipped func _mm256_maskz_unpackhi_epi64

   --  skipped func _mm_mask_unpacklo_epi32

   --  skipped func _mm_maskz_unpacklo_epi32

   --  skipped func _mm256_mask_unpacklo_epi32

   --  skipped func _mm256_maskz_unpacklo_epi32

   --  skipped func _mm_mask_unpacklo_epi64

   --  skipped func _mm_maskz_unpacklo_epi64

   --  skipped func _mm256_mask_unpacklo_epi64

   --  skipped func _mm256_maskz_unpacklo_epi64

   --  skipped func _mm_cmpeq_epu32_mask

   --  skipped func _mm_cmpeq_epi32_mask

   --  skipped func _mm_mask_cmpeq_epu32_mask

   --  skipped func _mm_mask_cmpeq_epi32_mask

   --  skipped func _mm256_cmpeq_epu32_mask

   --  skipped func _mm256_cmpeq_epi32_mask

   --  skipped func _mm256_mask_cmpeq_epu32_mask

   --  skipped func _mm256_mask_cmpeq_epi32_mask

   --  skipped func _mm_cmpeq_epu64_mask

   --  skipped func _mm_cmpeq_epi64_mask

   --  skipped func _mm_mask_cmpeq_epu64_mask

   --  skipped func _mm_mask_cmpeq_epi64_mask

   --  skipped func _mm256_cmpeq_epu64_mask

   --  skipped func _mm256_cmpeq_epi64_mask

   --  skipped func _mm256_mask_cmpeq_epu64_mask

   --  skipped func _mm256_mask_cmpeq_epi64_mask

   --  skipped func _mm_cmpgt_epu32_mask

   --  skipped func _mm_cmpgt_epi32_mask

   --  skipped func _mm_mask_cmpgt_epu32_mask

   --  skipped func _mm_mask_cmpgt_epi32_mask

   --  skipped func _mm256_cmpgt_epu32_mask

   --  skipped func _mm256_cmpgt_epi32_mask

   --  skipped func _mm256_mask_cmpgt_epu32_mask

   --  skipped func _mm256_mask_cmpgt_epi32_mask

   --  skipped func _mm_cmpgt_epu64_mask

   --  skipped func _mm_cmpgt_epi64_mask

   --  skipped func _mm_mask_cmpgt_epu64_mask

   --  skipped func _mm_mask_cmpgt_epi64_mask

   --  skipped func _mm256_cmpgt_epu64_mask

   --  skipped func _mm256_cmpgt_epi64_mask

   --  skipped func _mm256_mask_cmpgt_epu64_mask

   --  skipped func _mm256_mask_cmpgt_epi64_mask

   --  skipped func _mm_test_epi32_mask

   --  skipped func _mm_mask_test_epi32_mask

   --  skipped func _mm256_test_epi32_mask

   --  skipped func _mm256_mask_test_epi32_mask

   --  skipped func _mm_test_epi64_mask

   --  skipped func _mm_mask_test_epi64_mask

   --  skipped func _mm256_test_epi64_mask

   --  skipped func _mm256_mask_test_epi64_mask

   --  skipped func _mm_testn_epi32_mask

   --  skipped func _mm_mask_testn_epi32_mask

   --  skipped func _mm256_testn_epi32_mask

   --  skipped func _mm256_mask_testn_epi32_mask

   --  skipped func _mm_testn_epi64_mask

   --  skipped func _mm_mask_testn_epi64_mask

   --  skipped func _mm256_testn_epi64_mask

   --  skipped func _mm256_mask_testn_epi64_mask

   --  skipped func _mm256_mask_compress_pd

   --  skipped func _mm256_maskz_compress_pd

   --  skipped func _mm256_mask_compressstoreu_pd

   --  skipped func _mm_mask_compress_pd

   --  skipped func _mm_maskz_compress_pd

   --  skipped func _mm_mask_compressstoreu_pd

   --  skipped func _mm256_mask_compress_ps

   --  skipped func _mm256_maskz_compress_ps

   --  skipped func _mm256_mask_compressstoreu_ps

   --  skipped func _mm_mask_compress_ps

   --  skipped func _mm_maskz_compress_ps

   --  skipped func _mm_mask_compressstoreu_ps

   --  skipped func _mm256_mask_compress_epi64

   --  skipped func _mm256_maskz_compress_epi64

   --  skipped func _mm256_mask_compressstoreu_epi64

   --  skipped func _mm_mask_compress_epi64

   --  skipped func _mm_maskz_compress_epi64

   --  skipped func _mm_mask_compressstoreu_epi64

   --  skipped func _mm256_mask_compress_epi32

   --  skipped func _mm256_maskz_compress_epi32

   --  skipped func _mm256_mask_compressstoreu_epi32

   --  skipped func _mm_mask_compress_epi32

   --  skipped func _mm_maskz_compress_epi32

   --  skipped func _mm_mask_compressstoreu_epi32

   --  skipped func _mm256_mask_expand_pd

   --  skipped func _mm256_maskz_expand_pd

   --  skipped func _mm256_mask_expandloadu_pd

   --  skipped func _mm256_maskz_expandloadu_pd

   --  skipped func _mm_mask_expand_pd

   --  skipped func _mm_maskz_expand_pd

   --  skipped func _mm_mask_expandloadu_pd

   --  skipped func _mm_maskz_expandloadu_pd

   --  skipped func _mm256_mask_expand_ps

   --  skipped func _mm256_maskz_expand_ps

   --  skipped func _mm256_mask_expandloadu_ps

   --  skipped func _mm256_maskz_expandloadu_ps

   --  skipped func _mm_mask_expand_ps

   --  skipped func _mm_maskz_expand_ps

   --  skipped func _mm_mask_expandloadu_ps

   --  skipped func _mm_maskz_expandloadu_ps

   --  skipped func _mm256_mask_expand_epi64

   --  skipped func _mm256_maskz_expand_epi64

   --  skipped func _mm256_mask_expandloadu_epi64

   --  skipped func _mm256_maskz_expandloadu_epi64

   --  skipped func _mm_mask_expand_epi64

   --  skipped func _mm_maskz_expand_epi64

   --  skipped func _mm_mask_expandloadu_epi64

   --  skipped func _mm_maskz_expandloadu_epi64

   --  skipped func _mm256_mask_expand_epi32

   --  skipped func _mm256_maskz_expand_epi32

   --  skipped func _mm256_mask_expandloadu_epi32

   --  skipped func _mm256_maskz_expandloadu_epi32

   --  skipped func _mm_mask_expand_epi32

   --  skipped func _mm_maskz_expand_epi32

   --  skipped func _mm_mask_expandloadu_epi32

   --  skipped func _mm_maskz_expandloadu_epi32

   --  skipped func _mm256_permutex2var_pd

  -- idx  
   --  skipped func _mm256_mask_permutex2var_pd

  -- idx  
   --  skipped func _mm256_mask2_permutex2var_pd

  -- idx  
   --  skipped func _mm256_maskz_permutex2var_pd

  -- idx  
   --  skipped func _mm256_permutex2var_ps

  -- idx  
   --  skipped func _mm256_mask_permutex2var_ps

  -- idx  
   --  skipped func _mm256_mask2_permutex2var_ps

  -- idx  
   --  skipped func _mm256_maskz_permutex2var_ps

  -- idx  
   --  skipped func _mm_permutex2var_epi64

  -- idx  
   --  skipped func _mm_mask_permutex2var_epi64

  -- idx  
   --  skipped func _mm_mask2_permutex2var_epi64

  -- idx  
   --  skipped func _mm_maskz_permutex2var_epi64

  -- idx  
   --  skipped func _mm_permutex2var_epi32

  -- idx  
   --  skipped func _mm_mask_permutex2var_epi32

  -- idx  
   --  skipped func _mm_mask2_permutex2var_epi32

  -- idx  
   --  skipped func _mm_maskz_permutex2var_epi32

  -- idx  
   --  skipped func _mm256_permutex2var_epi64

  -- idx  
   --  skipped func _mm256_mask_permutex2var_epi64

  -- idx  
   --  skipped func _mm256_mask2_permutex2var_epi64

  -- idx  
   --  skipped func _mm256_maskz_permutex2var_epi64

  -- idx  
   --  skipped func _mm256_permutex2var_epi32

  -- idx  
   --  skipped func _mm256_mask_permutex2var_epi32

  -- idx  
   --  skipped func _mm256_mask2_permutex2var_epi32

  -- idx  
   --  skipped func _mm256_maskz_permutex2var_epi32

  -- idx  
   --  skipped func _mm_permutex2var_pd

  -- idx  
   --  skipped func _mm_mask_permutex2var_pd

  -- idx  
   --  skipped func _mm_mask2_permutex2var_pd

  -- idx  
   --  skipped func _mm_maskz_permutex2var_pd

  -- idx  
   --  skipped func _mm_permutex2var_ps

  -- idx  
   --  skipped func _mm_mask_permutex2var_ps

  -- idx  
   --  skipped func _mm_mask2_permutex2var_ps

  -- idx  
   --  skipped func _mm_maskz_permutex2var_ps

  -- idx  
   --  skipped func _mm_srav_epi64

   --  skipped func _mm_mask_srav_epi64

   --  skipped func _mm_maskz_srav_epi64

   --  skipped func _mm256_mask_sllv_epi32

   --  skipped func _mm256_maskz_sllv_epi32

   --  skipped func _mm_mask_sllv_epi32

   --  skipped func _mm_maskz_sllv_epi32

   --  skipped func _mm256_mask_sllv_epi64

   --  skipped func _mm256_maskz_sllv_epi64

   --  skipped func _mm_mask_sllv_epi64

   --  skipped func _mm_maskz_sllv_epi64

   --  skipped func _mm256_mask_srav_epi32

   --  skipped func _mm256_maskz_srav_epi32

   --  skipped func _mm_mask_srav_epi32

   --  skipped func _mm_maskz_srav_epi32

   --  skipped func _mm256_mask_srlv_epi32

   --  skipped func _mm256_maskz_srlv_epi32

   --  skipped func _mm_mask_srlv_epi32

   --  skipped func _mm_maskz_srlv_epi32

   --  skipped func _mm256_mask_srlv_epi64

   --  skipped func _mm256_maskz_srlv_epi64

   --  skipped func _mm_mask_srlv_epi64

   --  skipped func _mm_maskz_srlv_epi64

   --  skipped func _mm256_rolv_epi32

   --  skipped func _mm256_mask_rolv_epi32

   --  skipped func _mm256_maskz_rolv_epi32

   --  skipped func _mm_rolv_epi32

   --  skipped func _mm_mask_rolv_epi32

   --  skipped func _mm_maskz_rolv_epi32

   --  skipped func _mm256_rorv_epi32

   --  skipped func _mm256_mask_rorv_epi32

   --  skipped func _mm256_maskz_rorv_epi32

   --  skipped func _mm_rorv_epi32

   --  skipped func _mm_mask_rorv_epi32

   --  skipped func _mm_maskz_rorv_epi32

   --  skipped func _mm256_rolv_epi64

   --  skipped func _mm256_mask_rolv_epi64

   --  skipped func _mm256_maskz_rolv_epi64

   --  skipped func _mm_rolv_epi64

   --  skipped func _mm_mask_rolv_epi64

   --  skipped func _mm_maskz_rolv_epi64

   --  skipped func _mm256_rorv_epi64

   --  skipped func _mm256_mask_rorv_epi64

   --  skipped func _mm256_maskz_rorv_epi64

   --  skipped func _mm_rorv_epi64

   --  skipped func _mm_mask_rorv_epi64

   --  skipped func _mm_maskz_rorv_epi64

   --  skipped func _mm256_srav_epi64

   --  skipped func _mm256_mask_srav_epi64

   --  skipped func _mm256_maskz_srav_epi64

   --  skipped func _mm256_mask_and_epi64

   --  skipped func _mm256_maskz_and_epi64

   --  skipped func _mm_mask_and_epi64

   --  skipped func _mm_maskz_and_epi64

   --  skipped func _mm256_mask_andnot_epi64

   --  skipped func _mm256_maskz_andnot_epi64

   --  skipped func _mm_mask_andnot_epi64

   --  skipped func _mm_maskz_andnot_epi64

   --  skipped func _mm256_mask_or_epi64

   --  skipped func _mm256_maskz_or_epi64

   --  skipped func _mm_mask_or_epi64

   --  skipped func _mm_maskz_or_epi64

   --  skipped func _mm256_mask_xor_epi64

   --  skipped func _mm256_maskz_xor_epi64

   --  skipped func _mm_mask_xor_epi64

   --  skipped func _mm_maskz_xor_epi64

   --  skipped func _mm256_mask_max_pd

   --  skipped func _mm256_maskz_max_pd

   --  skipped func _mm256_mask_max_ps

   --  skipped func _mm256_maskz_max_ps

   --  skipped func _mm_mask_div_ps

   --  skipped func _mm_maskz_div_ps

   --  skipped func _mm_mask_div_pd

   --  skipped func _mm_maskz_div_pd

   --  skipped func _mm256_mask_min_pd

   --  skipped func _mm256_mask_div_pd

   --  skipped func _mm256_maskz_min_pd

   --  skipped func _mm256_mask_min_ps

   --  skipped func _mm256_maskz_div_pd

   --  skipped func _mm256_mask_div_ps

   --  skipped func _mm256_maskz_min_ps

   --  skipped func _mm256_maskz_div_ps

   --  skipped func _mm_mask_min_ps

   --  skipped func _mm_mask_mul_ps

   --  skipped func _mm_maskz_min_ps

   --  skipped func _mm_maskz_mul_ps

   --  skipped func _mm_mask_max_ps

   --  skipped func _mm_maskz_max_ps

   --  skipped func _mm_mask_min_pd

   --  skipped func _mm_maskz_min_pd

   --  skipped func _mm_mask_max_pd

   --  skipped func _mm_maskz_max_pd

   --  skipped func _mm_mask_mul_pd

   --  skipped func _mm_maskz_mul_pd

   --  skipped func _mm256_mask_mul_ps

   --  skipped func _mm256_maskz_mul_ps

   --  skipped func _mm256_mask_mul_pd

   --  skipped func _mm256_maskz_mul_pd

   --  skipped func _mm256_maskz_max_epi64

   --  skipped func _mm256_mask_max_epi64

   --  skipped func _mm256_min_epi64

   --  skipped func _mm256_mask_min_epi64

   --  skipped func _mm256_maskz_min_epi64

   --  skipped func _mm256_maskz_max_epu64

   --  skipped func _mm256_max_epi64

   --  skipped func _mm256_max_epu64

   --  skipped func _mm256_mask_max_epu64

   --  skipped func _mm256_min_epu64

   --  skipped func _mm256_mask_min_epu64

   --  skipped func _mm256_maskz_min_epu64

   --  skipped func _mm256_maskz_max_epi32

   --  skipped func _mm256_mask_max_epi32

   --  skipped func _mm256_maskz_min_epi32

   --  skipped func _mm256_mask_min_epi32

   --  skipped func _mm256_maskz_max_epu32

   --  skipped func _mm256_mask_max_epu32

   --  skipped func _mm256_maskz_min_epu32

   --  skipped func _mm256_mask_min_epu32

   --  skipped func _mm_maskz_max_epi64

   --  skipped func _mm_mask_max_epi64

   --  skipped func _mm_min_epi64

   --  skipped func _mm_mask_min_epi64

   --  skipped func _mm_maskz_min_epi64

   --  skipped func _mm_maskz_max_epu64

   --  skipped func _mm_max_epi64

   --  skipped func _mm_max_epu64

   --  skipped func _mm_mask_max_epu64

   --  skipped func _mm_min_epu64

   --  skipped func _mm_mask_min_epu64

   --  skipped func _mm_maskz_min_epu64

   --  skipped func _mm_maskz_max_epi32

   --  skipped func _mm_mask_max_epi32

   --  skipped func _mm_maskz_min_epi32

   --  skipped func _mm_mask_min_epi32

   --  skipped func _mm_maskz_max_epu32

   --  skipped func _mm_mask_max_epu32

   --  skipped func _mm_maskz_min_epu32

   --  skipped func _mm_mask_min_epu32

   --  skipped func _mm_broadcastmb_epi64

   --  skipped func _mm256_broadcastmb_epi64

   --  skipped func _mm_broadcastmw_epi32

   --  skipped func _mm256_broadcastmw_epi32

   --  skipped func _mm256_lzcnt_epi32

   --  skipped func _mm256_mask_lzcnt_epi32

   --  skipped func _mm256_maskz_lzcnt_epi32

   --  skipped func _mm256_lzcnt_epi64

   --  skipped func _mm256_mask_lzcnt_epi64

   --  skipped func _mm256_maskz_lzcnt_epi64

   --  skipped func _mm256_conflict_epi64

   --  skipped func _mm256_mask_conflict_epi64

   --  skipped func _mm256_maskz_conflict_epi64

   --  skipped func _mm256_conflict_epi32

   --  skipped func _mm256_mask_conflict_epi32

   --  skipped func _mm256_maskz_conflict_epi32

   --  skipped func _mm_lzcnt_epi32

   --  skipped func _mm_mask_lzcnt_epi32

   --  skipped func _mm_maskz_lzcnt_epi32

   --  skipped func _mm_lzcnt_epi64

   --  skipped func _mm_mask_lzcnt_epi64

   --  skipped func _mm_maskz_lzcnt_epi64

   --  skipped func _mm_conflict_epi64

   --  skipped func _mm_mask_conflict_epi64

   --  skipped func _mm_maskz_conflict_epi64

   --  skipped func _mm_conflict_epi32

   --  skipped func _mm_mask_conflict_epi32

   --  skipped func _mm_maskz_conflict_epi32

   --  skipped func _mm256_mask_unpacklo_pd

   --  skipped func _mm256_maskz_unpacklo_pd

   --  skipped func _mm_mask_unpacklo_pd

   --  skipped func _mm_maskz_unpacklo_pd

   --  skipped func _mm256_mask_unpacklo_ps

   --  skipped func _mm256_mask_unpackhi_pd

   --  skipped func _mm256_maskz_unpackhi_pd

   --  skipped func _mm_mask_unpackhi_pd

   --  skipped func _mm_maskz_unpackhi_pd

   --  skipped func _mm256_mask_unpackhi_ps

   --  skipped func _mm256_maskz_unpackhi_ps

   --  skipped func _mm_mask_unpackhi_ps

   --  skipped func _mm_maskz_unpackhi_ps

   --  skipped func _mm_mask_cvtph_ps

   --  skipped func _mm_maskz_cvtph_ps

   --  skipped func _mm256_maskz_unpacklo_ps

   --  skipped func _mm256_mask_cvtph_ps

   --  skipped func _mm256_maskz_cvtph_ps

   --  skipped func _mm_mask_unpacklo_ps

   --  skipped func _mm_maskz_unpacklo_ps

   --  skipped func _mm256_mask_sra_epi32

   --  skipped func _mm256_maskz_sra_epi32

   --  skipped func _mm_mask_sra_epi32

   --  skipped func _mm_maskz_sra_epi32

   --  skipped func _mm256_sra_epi64

   --  skipped func _mm256_mask_sra_epi64

   --  skipped func _mm256_maskz_sra_epi64

   --  skipped func _mm_sra_epi64

   --  skipped func _mm_mask_sra_epi64

   --  skipped func _mm_maskz_sra_epi64

   --  skipped func _mm_mask_sll_epi32

   --  skipped func _mm_maskz_sll_epi32

   --  skipped func _mm_mask_sll_epi64

   --  skipped func _mm_maskz_sll_epi64

   --  skipped func _mm256_mask_sll_epi32

   --  skipped func _mm256_maskz_sll_epi32

   --  skipped func _mm256_mask_sll_epi64

   --  skipped func _mm256_maskz_sll_epi64

   --  skipped func _mm256_mask_permutexvar_ps

   --  skipped func _mm256_maskz_permutexvar_ps

   --  skipped func _mm256_permutexvar_pd

   --  skipped func _mm256_mask_permutexvar_pd

   --  skipped func _mm256_maskz_permutexvar_pd

   --  skipped func _mm256_mask_permutevar_pd

   --  skipped func _mm256_maskz_permutevar_pd

   --  skipped func _mm256_mask_permutevar_ps

   --  skipped func _mm256_maskz_permutevar_ps

   --  skipped func _mm_mask_permutevar_pd

   --  skipped func _mm_maskz_permutevar_pd

   --  skipped func _mm_mask_permutevar_ps

   --  skipped func _mm_maskz_permutevar_ps

   --  skipped func _mm256_maskz_mullo_epi32

   --  skipped func _mm256_maskz_permutexvar_epi64

   --  skipped func _mm256_mask_mullo_epi32

   --  skipped func _mm_maskz_mullo_epi32

   --  skipped func _mm_mask_mullo_epi32

   --  skipped func _mm256_mask_mul_epi32

   --  skipped func _mm256_maskz_mul_epi32

   --  skipped func _mm_mask_mul_epi32

   --  skipped func _mm_maskz_mul_epi32

   --  skipped func _mm256_permutexvar_epi64

   --  skipped func _mm256_mask_permutexvar_epi64

   --  skipped func _mm256_mask_mul_epu32

   --  skipped func _mm256_maskz_permutexvar_epi32

   --  skipped func _mm256_maskz_mul_epu32

   --  skipped func _mm_mask_mul_epu32

   --  skipped func _mm_maskz_mul_epu32

   --  skipped func _mm256_permutexvar_epi32

   --  skipped func _mm256_mask_permutexvar_epi32

   --  skipped func _mm256_mask_cmpneq_epu32_mask

   --  skipped func _mm256_cmpneq_epu32_mask

   --  skipped func _mm256_mask_cmplt_epu32_mask

   --  skipped func _mm256_cmplt_epu32_mask

   --  skipped func _mm256_mask_cmpge_epu32_mask

   --  skipped func _mm256_cmpge_epu32_mask

   --  skipped func _mm256_mask_cmple_epu32_mask

   --  skipped func _mm256_cmple_epu32_mask

   --  skipped func _mm256_mask_cmpneq_epu64_mask

   --  skipped func _mm256_cmpneq_epu64_mask

   --  skipped func _mm256_mask_cmplt_epu64_mask

   --  skipped func _mm256_cmplt_epu64_mask

   --  skipped func _mm256_mask_cmpge_epu64_mask

   --  skipped func _mm256_cmpge_epu64_mask

   --  skipped func _mm256_mask_cmple_epu64_mask

   --  skipped func _mm256_cmple_epu64_mask

   --  skipped func _mm256_mask_cmpneq_epi32_mask

   --  skipped func _mm256_cmpneq_epi32_mask

   --  skipped func _mm256_mask_cmplt_epi32_mask

   --  skipped func _mm256_cmplt_epi32_mask

   --  skipped func _mm256_mask_cmpge_epi32_mask

   --  skipped func _mm256_cmpge_epi32_mask

   --  skipped func _mm256_mask_cmple_epi32_mask

   --  skipped func _mm256_cmple_epi32_mask

   --  skipped func _mm256_mask_cmpneq_epi64_mask

   --  skipped func _mm256_cmpneq_epi64_mask

   --  skipped func _mm256_mask_cmplt_epi64_mask

   --  skipped func _mm256_cmplt_epi64_mask

   --  skipped func _mm256_mask_cmpge_epi64_mask

   --  skipped func _mm256_cmpge_epi64_mask

   --  skipped func _mm256_mask_cmple_epi64_mask

   --  skipped func _mm256_cmple_epi64_mask

   --  skipped func _mm_mask_cmpneq_epu32_mask

   --  skipped func _mm_cmpneq_epu32_mask

   --  skipped func _mm_mask_cmplt_epu32_mask

   --  skipped func _mm_cmplt_epu32_mask

   --  skipped func _mm_mask_cmpge_epu32_mask

   --  skipped func _mm_cmpge_epu32_mask

   --  skipped func _mm_mask_cmple_epu32_mask

   --  skipped func _mm_cmple_epu32_mask

   --  skipped func _mm_mask_cmpneq_epu64_mask

   --  skipped func _mm_cmpneq_epu64_mask

   --  skipped func _mm_mask_cmplt_epu64_mask

   --  skipped func _mm_cmplt_epu64_mask

   --  skipped func _mm_mask_cmpge_epu64_mask

   --  skipped func _mm_cmpge_epu64_mask

   --  skipped func _mm_mask_cmple_epu64_mask

   --  skipped func _mm_cmple_epu64_mask

   --  skipped func _mm_mask_cmpneq_epi32_mask

   --  skipped func _mm_cmpneq_epi32_mask

   --  skipped func _mm_mask_cmplt_epi32_mask

   --  skipped func _mm_cmplt_epi32_mask

   --  skipped func _mm_mask_cmpge_epi32_mask

   --  skipped func _mm_cmpge_epi32_mask

   --  skipped func _mm_mask_cmple_epi32_mask

   --  skipped func _mm_cmple_epi32_mask

   --  skipped func _mm_mask_cmpneq_epi64_mask

   --  skipped func _mm_cmpneq_epi64_mask

   --  skipped func _mm_mask_cmplt_epi64_mask

   --  skipped func _mm_cmplt_epi64_mask

   --  skipped func _mm_mask_cmpge_epi64_mask

   --  skipped func _mm_cmpge_epi64_mask

   --  skipped func _mm_mask_cmple_epi64_mask

   --  skipped func _mm_cmple_epi64_mask

end avx512vlintrin_h;
