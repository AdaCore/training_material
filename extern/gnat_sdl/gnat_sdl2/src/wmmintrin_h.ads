pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package wmmintrin_h is

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
  --   User Guide and Reference, version 10.1.   

  -- We need definitions from the SSE2 header file.   
  -- AES  
  -- Performs 1 round of AES decryption of the first m128i using 
  --   the second m128i as a round key.   

   --  skipped func _mm_aesdec_si128

  -- Performs the last round of AES decryption of the first m128i 
  --   using the second m128i as a round key.   

   --  skipped func _mm_aesdeclast_si128

  -- Performs 1 round of AES encryption of the first m128i using 
  --   the second m128i as a round key.   

   --  skipped func _mm_aesenc_si128

  -- Performs the last round of AES encryption of the first m128i
  --   using the second m128i as a round key.   

   --  skipped func _mm_aesenclast_si128

  -- Performs the InverseMixColumn operation on the source m128i 
  --   and stores the result into m128i destination.   

   --  skipped func _mm_aesimc_si128

  -- Generates a m128i round key for the input m128i AES cipher key and
  --   byte round constant.  The second parameter must be a compile time
  --   constant.   

  -- PCLMUL  
  -- Performs carry-less integer multiplication of 64-bit halves of
  --   128-bit input operands.  The third parameter inducates which 64-bit
  --   haves of the input parameters v1 and v2 should be used. It must be
  --   a compile time constant.   

end wmmintrin_h;
