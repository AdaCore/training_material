pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package avx512dqintrin_h is

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

   --  skipped func _ktest_mask8_u8

   --  skipped func _ktestz_mask8_u8

   --  skipped func _ktestc_mask8_u8

   --  skipped func _ktest_mask16_u8

   --  skipped func _ktestz_mask16_u8

   --  skipped func _ktestc_mask16_u8

   --  skipped func _kortest_mask8_u8

   --  skipped func _kortestz_mask8_u8

   --  skipped func _kortestc_mask8_u8

   --  skipped func _kadd_mask8

   --  skipped func _kadd_mask16

   --  skipped func _cvtmask8_u32

   --  skipped func _cvtu32_mask8

   --  skipped func _load_mask8

   --  skipped func _store_mask8

   --  skipped func _knot_mask8

   --  skipped func _kor_mask8

   --  skipped func _kxnor_mask8

   --  skipped func _kxor_mask8

   --  skipped func _kand_mask8

   --  skipped func _kandn_mask8

   --  skipped func _mm512_broadcast_f64x2

   --  skipped func _mm512_mask_broadcast_f64x2

   --  skipped func _mm512_maskz_broadcast_f64x2

   --  skipped func _mm512_broadcast_i64x2

   --  skipped func _mm512_mask_broadcast_i64x2

   --  skipped func _mm512_maskz_broadcast_i64x2

   --  skipped func _mm512_broadcast_f32x2

   --  skipped func _mm512_mask_broadcast_f32x2

   --  skipped func _mm512_maskz_broadcast_f32x2

   --  skipped func _mm512_broadcast_i32x2

   --  skipped func _mm512_mask_broadcast_i32x2

   --  skipped func _mm512_maskz_broadcast_i32x2

   --  skipped func _mm512_broadcast_f32x8

   --  skipped func _mm512_mask_broadcast_f32x8

   --  skipped func _mm512_maskz_broadcast_f32x8

   --  skipped func _mm512_broadcast_i32x8

   --  skipped func _mm512_mask_broadcast_i32x8

   --  skipped func _mm512_maskz_broadcast_i32x8

   --  skipped func _mm512_mullo_epi64

   --  skipped func _mm512_mask_mullo_epi64

   --  skipped func _mm512_maskz_mullo_epi64

   --  skipped func _mm512_xor_pd

   --  skipped func _mm512_mask_xor_pd

   --  skipped func _mm512_maskz_xor_pd

   --  skipped func _mm512_xor_ps

   --  skipped func _mm512_mask_xor_ps

   --  skipped func _mm512_maskz_xor_ps

   --  skipped func _mm512_or_pd

   --  skipped func _mm512_mask_or_pd

   --  skipped func _mm512_maskz_or_pd

   --  skipped func _mm512_or_ps

   --  skipped func _mm512_mask_or_ps

   --  skipped func _mm512_maskz_or_ps

   --  skipped func _mm512_and_pd

   --  skipped func _mm512_mask_and_pd

   --  skipped func _mm512_maskz_and_pd

   --  skipped func _mm512_and_ps

   --  skipped func _mm512_mask_and_ps

   --  skipped func _mm512_maskz_and_ps

   --  skipped func _mm512_andnot_pd

   --  skipped func _mm512_mask_andnot_pd

   --  skipped func _mm512_maskz_andnot_pd

   --  skipped func _mm512_andnot_ps

   --  skipped func _mm512_mask_andnot_ps

   --  skipped func _mm512_maskz_andnot_ps

   --  skipped func _mm512_movepi32_mask

   --  skipped func _mm512_movepi64_mask

   --  skipped func _mm512_movm_epi32

   --  skipped func _mm512_movm_epi64

   --  skipped func _mm512_cvttpd_epi64

   --  skipped func _mm512_mask_cvttpd_epi64

   --  skipped func _mm512_maskz_cvttpd_epi64

   --  skipped func _mm512_cvttpd_epu64

   --  skipped func _mm512_mask_cvttpd_epu64

   --  skipped func _mm512_maskz_cvttpd_epu64

   --  skipped func _mm512_cvttps_epi64

   --  skipped func _mm512_mask_cvttps_epi64

   --  skipped func _mm512_maskz_cvttps_epi64

   --  skipped func _mm512_cvttps_epu64

   --  skipped func _mm512_mask_cvttps_epu64

   --  skipped func _mm512_maskz_cvttps_epu64

   --  skipped func _mm512_cvtpd_epi64

   --  skipped func _mm512_mask_cvtpd_epi64

   --  skipped func _mm512_maskz_cvtpd_epi64

   --  skipped func _mm512_cvtpd_epu64

   --  skipped func _mm512_mask_cvtpd_epu64

   --  skipped func _mm512_maskz_cvtpd_epu64

   --  skipped func _mm512_cvtps_epi64

   --  skipped func _mm512_mask_cvtps_epi64

   --  skipped func _mm512_maskz_cvtps_epi64

   --  skipped func _mm512_cvtps_epu64

   --  skipped func _mm512_mask_cvtps_epu64

   --  skipped func _mm512_maskz_cvtps_epu64

   --  skipped func _mm512_cvtepi64_ps

   --  skipped func _mm512_mask_cvtepi64_ps

   --  skipped func _mm512_maskz_cvtepi64_ps

   --  skipped func _mm512_cvtepu64_ps

   --  skipped func _mm512_mask_cvtepu64_ps

   --  skipped func _mm512_maskz_cvtepu64_ps

   --  skipped func _mm512_cvtepi64_pd

   --  skipped func _mm512_mask_cvtepi64_pd

   --  skipped func _mm512_maskz_cvtepi64_pd

   --  skipped func _mm512_cvtepu64_pd

   --  skipped func _mm512_mask_cvtepu64_pd

   --  skipped func _mm512_maskz_cvtepu64_pd

end avx512dqintrin_h;
