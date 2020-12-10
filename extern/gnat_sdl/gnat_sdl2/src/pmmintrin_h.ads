pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package pmmintrin_h is

  -- Copyright (C) 2003-2017 Free Software Foundation, Inc.
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
  --   User Guide and Reference, version 9.0.   

  -- We need definitions from the SSE2 and SSE header files 
  -- Additional bits in the MXCSR.   
   --  skipped func _mm_addsub_ps

   --  skipped func _mm_hadd_ps

   --  skipped func _mm_hsub_ps

   --  skipped func _mm_movehdup_ps

   --  skipped func _mm_moveldup_ps

   --  skipped func _mm_addsub_pd

   --  skipped func _mm_hadd_pd

   --  skipped func _mm_hsub_pd

   --  skipped func _mm_loaddup_pd

   --  skipped func _mm_movedup_pd

   --  skipped func _mm_lddqu_si128

   --  skipped func _mm_monitor

   --  skipped func _mm_mwait

end pmmintrin_h;
