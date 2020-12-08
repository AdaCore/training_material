pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package ia32intrin_h is

  -- Copyright (C) 2009-2017 Free Software Foundation, Inc.
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

  -- 32bit bsf  
   --  skipped func __bsfd

  -- 32bit bsr  
   --  skipped func __bsrd

  -- 32bit bswap  
   --  skipped func __bswapd

  -- 32bit accumulate CRC32 (polynomial 0x11EDC6F41) value.   
   --  skipped func __crc32b

   --  skipped func __crc32w

   --  skipped func __crc32d

  -- 32bit popcnt  
   --  skipped func __popcntd

  -- rdpmc  
   --  skipped func __rdpmc

  -- rdtsc  
   --  skipped func __rdtsc

  -- rdtscp  
   --  skipped func __rdtscp

  -- 8bit rol  
   --  skipped func __rolb

  -- 16bit rol  
   --  skipped func __rolw

  -- 32bit rol  
   --  skipped func __rold

  -- 8bit ror  
   --  skipped func __rorb

  -- 16bit ror  
   --  skipped func __rorw

  -- 32bit ror  
   --  skipped func __rord

  -- Pause  
   --  skipped func __pause

  -- 64bit bsf  
   --  skipped func __bsfq

  -- 64bit bsr  
   --  skipped func __bsrq

  -- 64bit bswap  
   --  skipped func __bswapq

  -- 64bit accumulate CRC32 (polynomial 0x11EDC6F41) value.   
   --  skipped func __crc32q

  -- 64bit popcnt  
   --  skipped func __popcntq

  -- 64bit rol  
   --  skipped func __rolq

  -- 64bit ror  
   --  skipped func __rorq

  -- Read flags register  
   --  skipped func __readeflags

  -- Write flags register  
   --  skipped func __writeeflags

  -- Read flags register  
  -- Write flags register  
  -- On LP64 systems, longs are 64-bit.  Use the appropriate rotate
  -- * function.   

end ia32intrin_h;
