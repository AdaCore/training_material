pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package avx512cdintrin_h is

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
   subtype uu_v8di is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avx512cdintrin.h:38

   subtype uu_v16si is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avx512cdintrin.h:39

  -- The Intel API is flexible enough that we must allow aliasing with other
  --   vector types, and their scalar components.   

   subtype uu_m512i is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avx512cdintrin.h:43

   subtype uu_m512d is <vector>;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avx512cdintrin.h:44

   subtype uu_mmask8 is unsigned_char;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avx512cdintrin.h:46

   subtype uu_mmask16 is unsigned_short;  -- d:\install\gpl2018\lib\gcc\x86_64-pc-mingw32\7.3.1\include\avx512cdintrin.h:47

   --  skipped func _mm512_conflict_epi32

   --  skipped func _mm512_mask_conflict_epi32

   --  skipped func _mm512_maskz_conflict_epi32

   --  skipped func _mm512_conflict_epi64

   --  skipped func _mm512_mask_conflict_epi64

   --  skipped func _mm512_maskz_conflict_epi64

   --  skipped func _mm512_lzcnt_epi64

   --  skipped func _mm512_mask_lzcnt_epi64

   --  skipped func _mm512_maskz_lzcnt_epi64

   --  skipped func _mm512_lzcnt_epi32

   --  skipped func _mm512_mask_lzcnt_epi32

   --  skipped func _mm512_maskz_lzcnt_epi32

   --  skipped func _mm512_broadcastmb_epi64

   --  skipped func _mm512_broadcastmw_epi32

end avx512cdintrin_h;
