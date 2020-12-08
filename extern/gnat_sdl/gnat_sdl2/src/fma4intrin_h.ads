pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package fma4intrin_h is

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

  -- We need definitions from the SSE4A, SSE3, SSE2 and SSE header files.   
  -- 128b Floating point multiply/add type instructions.   
   --  skipped func _mm_macc_ps

   --  skipped func _mm_macc_pd

   --  skipped func _mm_macc_ss

   --  skipped func _mm_macc_sd

   --  skipped func _mm_msub_ps

   --  skipped func _mm_msub_pd

   --  skipped func _mm_msub_ss

   --  skipped func _mm_msub_sd

   --  skipped func _mm_nmacc_ps

   --  skipped func _mm_nmacc_pd

   --  skipped func _mm_nmacc_ss

   --  skipped func _mm_nmacc_sd

   --  skipped func _mm_nmsub_ps

   --  skipped func _mm_nmsub_pd

   --  skipped func _mm_nmsub_ss

   --  skipped func _mm_nmsub_sd

   --  skipped func _mm_maddsub_ps

   --  skipped func _mm_maddsub_pd

   --  skipped func _mm_msubadd_ps

   --  skipped func _mm_msubadd_pd

  -- 256b Floating point multiply/add type instructions.   
   --  skipped func _mm256_macc_ps

   --  skipped func _mm256_macc_pd

   --  skipped func _mm256_msub_ps

   --  skipped func _mm256_msub_pd

   --  skipped func _mm256_nmacc_ps

   --  skipped func _mm256_nmacc_pd

   --  skipped func _mm256_nmsub_ps

   --  skipped func _mm256_nmsub_pd

   --  skipped func _mm256_maddsub_ps

   --  skipped func _mm256_maddsub_pd

   --  skipped func _mm256_msubadd_ps

   --  skipped func _mm256_msubadd_pd

end fma4intrin_h;
