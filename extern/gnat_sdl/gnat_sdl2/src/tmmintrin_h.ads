pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package tmmintrin_h is

  -- Copyright (C) 2006-2017 Free Software Foundation, Inc.
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
  --   User Guide and Reference, version 9.1.   

  -- We need definitions from the SSE3, SSE2 and SSE header files 
   --  skipped func _mm_hadd_epi16

   --  skipped func _mm_hadd_epi32

   --  skipped func _mm_hadds_epi16

   --  skipped func _mm_hadd_pi16

   --  skipped func _mm_hadd_pi32

   --  skipped func _mm_hadds_pi16

   --  skipped func _mm_hsub_epi16

   --  skipped func _mm_hsub_epi32

   --  skipped func _mm_hsubs_epi16

   --  skipped func _mm_hsub_pi16

   --  skipped func _mm_hsub_pi32

   --  skipped func _mm_hsubs_pi16

   --  skipped func _mm_maddubs_epi16

   --  skipped func _mm_maddubs_pi16

   --  skipped func _mm_mulhrs_epi16

   --  skipped func _mm_mulhrs_pi16

   --  skipped func _mm_shuffle_epi8

   --  skipped func _mm_shuffle_pi8

   --  skipped func _mm_sign_epi8

   --  skipped func _mm_sign_epi16

   --  skipped func _mm_sign_epi32

   --  skipped func _mm_sign_pi8

   --  skipped func _mm_sign_pi16

   --  skipped func _mm_sign_pi32

   --  skipped func _mm_abs_epi8

   --  skipped func _mm_abs_epi16

   --  skipped func _mm_abs_epi32

   --  skipped func _mm_abs_pi8

   --  skipped func _mm_abs_pi16

   --  skipped func _mm_abs_pi32

end tmmintrin_h;
