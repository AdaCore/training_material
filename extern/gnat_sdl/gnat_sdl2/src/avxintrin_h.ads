pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package avxintrin_h is

  -- Copyright (C) 2008-2017 Free Software Foundation, Inc.
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
  --   User Guide and Reference, version 11.0.   

  -- Internal data types for implementing the intrinsics.   
   subtype uu_v4df is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avxintrin.h:41

   subtype uu_v8sf is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avxintrin.h:42

   subtype uu_v4di is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avxintrin.h:43

   subtype uu_v4du is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avxintrin.h:44

   subtype uu_v8si is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avxintrin.h:45

   subtype uu_v8su is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avxintrin.h:46

   subtype uu_v16hi is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avxintrin.h:47

   subtype uu_v16hu is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avxintrin.h:48

   subtype uu_v32qi is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avxintrin.h:49

   subtype uu_v32qu is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avxintrin.h:50

  -- The Intel API is flexible enough that we must allow aliasing with other
  --   vector types, and their scalar components.   

   subtype uu_m256 is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avxintrin.h:54

   subtype uu_m256i is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avxintrin.h:56

   subtype uu_m256d is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avxintrin.h:58

  -- Unaligned version of the same types.   
   subtype uu_m256_u is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avxintrin.h:62

   subtype uu_m256i_u is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avxintrin.h:65

   subtype uu_m256d_u is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avxintrin.h:68

  -- Compare predicates for scalar and packed compare intrinsics.   
  -- Equal (ordered, non-signaling)   
  -- Less-than (ordered, signaling)   
  -- Less-than-or-equal (ordered, signaling)   
  -- Unordered (non-signaling)   
  -- Not-equal (unordered, non-signaling)   
  -- Not-less-than (unordered, signaling)   
  -- Not-less-than-or-equal (unordered, signaling)   
  -- Ordered (nonsignaling)    
  -- Equal (unordered, non-signaling)   
  -- Not-greater-than-or-equal (unordered, signaling)   
  -- Not-greater-than (unordered, signaling)   
  -- False (ordered, non-signaling)   
  -- Not-equal (ordered, non-signaling)   
  -- Greater-than-or-equal (ordered, signaling)   
  -- Greater-than (ordered, signaling)   
  -- True (unordered, non-signaling)   
  -- Equal (ordered, signaling)   
  -- Less-than (ordered, non-signaling)   
  -- Less-than-or-equal (ordered, non-signaling)   
  -- Unordered (signaling)   
  -- Not-equal (unordered, signaling)   
  -- Not-less-than (unordered, non-signaling)   
  -- Not-less-than-or-equal (unordered, non-signaling)   
  -- Ordered (signaling)   
  -- Equal (unordered, signaling)   
  -- Not-greater-than-or-equal (unordered, non-signaling)   
  -- Not-greater-than (unordered, non-signaling)   
  -- False (ordered, signaling)   
  -- Not-equal (ordered, signaling)   
  -- Greater-than-or-equal (ordered, non-signaling)   
  -- Greater-than (ordered, non-signaling)   
  -- True (unordered, signaling)   
   --  skipped func _mm256_add_pd

   --  skipped func _mm256_add_ps

   --  skipped func _mm256_addsub_pd

   --  skipped func _mm256_addsub_ps

   --  skipped func _mm256_and_pd

   --  skipped func _mm256_and_ps

   --  skipped func _mm256_andnot_pd

   --  skipped func _mm256_andnot_ps

  -- Double/single precision floating point blend instructions - select
  --   data from 2 sources using constant/variable mask.   

   --  skipped func _mm256_blendv_pd

   --  skipped func _mm256_blendv_ps

   --  skipped func _mm256_div_pd

   --  skipped func _mm256_div_ps

  -- Dot product instructions with mask-defined summing and zeroing parts
  --   of result.   

   --  skipped func _mm256_hadd_pd

   --  skipped func _mm256_hadd_ps

   --  skipped func _mm256_hsub_pd

   --  skipped func _mm256_hsub_ps

   --  skipped func _mm256_max_pd

   --  skipped func _mm256_max_ps

   --  skipped func _mm256_min_pd

   --  skipped func _mm256_min_ps

   --  skipped func _mm256_mul_pd

   --  skipped func _mm256_mul_ps

   --  skipped func _mm256_or_pd

   --  skipped func _mm256_or_ps

   --  skipped func _mm256_sub_pd

   --  skipped func _mm256_sub_ps

   --  skipped func _mm256_xor_pd

   --  skipped func _mm256_xor_ps

   --  skipped func _mm256_cvtepi32_pd

   --  skipped func _mm256_cvtepi32_ps

   --  skipped func _mm256_cvtpd_ps

   --  skipped func _mm256_cvtps_epi32

   --  skipped func _mm256_cvtps_pd

   --  skipped func _mm256_cvttpd_epi32

   --  skipped func _mm256_cvtpd_epi32

   --  skipped func _mm256_cvttps_epi32

   --  skipped func _mm256_cvtsd_f64

   --  skipped func _mm256_cvtss_f32

   --  skipped func _mm256_zeroall

   --  skipped func _mm256_zeroupper

   --  skipped func _mm_permutevar_pd

   --  skipped func _mm256_permutevar_pd

   --  skipped func _mm_permutevar_ps

   --  skipped func _mm256_permutevar_ps

   --  skipped func _mm_broadcast_ss

   --  skipped func _mm256_broadcast_sd

   --  skipped func _mm256_broadcast_ss

   --  skipped func _mm256_broadcast_pd

   --  skipped func _mm256_broadcast_ps

   --  skipped func _mm256_load_pd

   --  skipped func _mm256_store_pd

   --  skipped func _mm256_load_ps

   --  skipped func _mm256_store_ps

   --  skipped func _mm256_loadu_pd

   --  skipped func _mm256_storeu_pd

   --  skipped func _mm256_loadu_ps

   --  skipped func _mm256_storeu_ps

   --  skipped func _mm256_load_si256

   --  skipped func _mm256_store_si256

   --  skipped func _mm256_loadu_si256

   --  skipped func _mm256_storeu_si256

   --  skipped func _mm_maskload_pd

   --  skipped func _mm_maskstore_pd

   --  skipped func _mm256_maskload_pd

   --  skipped func _mm256_maskstore_pd

   --  skipped func _mm_maskload_ps

   --  skipped func _mm_maskstore_ps

   --  skipped func _mm256_maskload_ps

   --  skipped func _mm256_maskstore_ps

   --  skipped func _mm256_movehdup_ps

   --  skipped func _mm256_moveldup_ps

   --  skipped func _mm256_movedup_pd

   --  skipped func _mm256_lddqu_si256

   --  skipped func _mm256_stream_si256

   --  skipped func _mm256_stream_pd

   --  skipped func _mm256_stream_ps

   --  skipped func _mm256_rcp_ps

   --  skipped func _mm256_rsqrt_ps

   --  skipped func _mm256_sqrt_pd

   --  skipped func _mm256_sqrt_ps

   --  skipped func _mm256_unpackhi_pd

   --  skipped func _mm256_unpacklo_pd

   --  skipped func _mm256_unpackhi_ps

   --  skipped func _mm256_unpacklo_ps

   --  skipped func _mm_testz_pd

   --  skipped func _mm_testc_pd

   --  skipped func _mm_testnzc_pd

   --  skipped func _mm_testz_ps

   --  skipped func _mm_testc_ps

   --  skipped func _mm_testnzc_ps

   --  skipped func _mm256_testz_pd

   --  skipped func _mm256_testc_pd

   --  skipped func _mm256_testnzc_pd

   --  skipped func _mm256_testz_ps

   --  skipped func _mm256_testc_ps

   --  skipped func _mm256_testnzc_ps

   --  skipped func _mm256_testz_si256

   --  skipped func _mm256_testc_si256

   --  skipped func _mm256_testnzc_si256

   --  skipped func _mm256_movemask_pd

   --  skipped func _mm256_movemask_ps

   --  skipped func _mm256_undefined_pd

   --  skipped func _mm256_undefined_ps

   --  skipped func _mm256_undefined_si256

   --  skipped func _mm256_setzero_pd

   --  skipped func _mm256_setzero_ps

   --  skipped func _mm256_setzero_si256

  -- Create the vector [A B C D].   
   --  skipped func _mm256_set_pd

  -- Create the vector [A B C D E F G H].   
   --  skipped func _mm256_set_ps

  -- Create the vector [A B C D E F G H].   
   --  skipped func _mm256_set_epi32

   --  skipped func _mm256_set_epi16

   --  skipped func _mm256_set_epi8

   --  skipped func _mm256_set_epi64x

  -- Create a vector with all elements equal to A.   
   --  skipped func _mm256_set1_pd

  -- Create a vector with all elements equal to A.   
   --  skipped func _mm256_set1_ps

  -- Create a vector with all elements equal to A.   
   --  skipped func _mm256_set1_epi32

   --  skipped func _mm256_set1_epi16

   --  skipped func _mm256_set1_epi8

   --  skipped func _mm256_set1_epi64x

  -- Create vectors of elements in the reversed order from the
  --   _mm256_set_XXX functions.   

   --  skipped func _mm256_setr_pd

   --  skipped func _mm256_setr_ps

   --  skipped func _mm256_setr_epi32

   --  skipped func _mm256_setr_epi16

   --  skipped func _mm256_setr_epi8

   --  skipped func _mm256_setr_epi64x

  -- Casts between various SP, DP, INT vector types.  Note that these do no
  --   conversion of values, they just change the type.   

   --  skipped func _mm256_castpd_ps

   --  skipped func _mm256_castpd_si256

   --  skipped func _mm256_castps_pd

   --  skipped func _mm256_castps_si256

   --  skipped func _mm256_castsi256_ps

   --  skipped func _mm256_castsi256_pd

   --  skipped func _mm256_castpd256_pd128

   --  skipped func _mm256_castps256_ps128

   --  skipped func _mm256_castsi256_si128

  -- When cast is done from a 128 to 256-bit type, the low 128 bits of
  --   the 256-bit result contain source parameter value and the upper 128
  --   bits of the result are undefined.  Those intrinsics shouldn't
  --   generate any extra moves.   

   --  skipped func _mm256_castpd128_pd256

   --  skipped func _mm256_castps128_ps256

   --  skipped func _mm256_castsi128_si256

end avxintrin_h;
