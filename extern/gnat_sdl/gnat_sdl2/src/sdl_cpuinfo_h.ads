pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_stdinc_h;

package SDL_cpuinfo_h is

   SDL_CACHELINE_SIZE : constant := 128;  --  ..\SDL2_tmp\SDL_cpuinfo.h:95

  --  Simple DirectMedia Layer
  --  Copyright (C) 1997-2018 Sam Lantinga <slouken@libsdl.org>
  --  This software is provided 'as-is', without any express or implied
  --  warranty.  In no event will the authors be held liable for any damages
  --  arising from the use of this software.
  --  Permission is granted to anyone to use this software for any purpose,
  --  including commercial applications, and to alter it and redistribute it
  --  freely, subject to the following restrictions:
  --  1. The origin of this software must not be misrepresented; you must not
  --     claim that you wrote the original software. If you use this software
  --     in a product, an acknowledgment in the product documentation would be
  --     appreciated but is not required.
  --  2. Altered source versions must be plainly marked as such, and must not be
  --     misrepresented as being the original software.
  --  3. This notice may not be removed or altered from any source distribution.
  -- 

  --*
  -- *  \file SDL_cpuinfo.h
  -- *
  -- *  CPU feature detection for SDL.
  --  

  -- Need to do this here because intrin.h has C++ code in it  
  -- Visual Studio 2005 has a bug where intrin.h conflicts with winnt.h  
  -- Many of the intrinsics SDL uses are not implemented by clang with Visual Studio  
  -- Set up for C function definitions, even when using C++  
  -- This is a guess for the cacheline size used for padding.
  -- * Most x86 processors have a 64 byte cache line.
  -- * The 64-bit PowerPC processors have a 128 byte cache line.
  -- * We'll use the larger value to be generally safe.
  --  

  --*
  -- *  This function returns the number of CPU cores available.
  --  

   function SDL_GetCPUCount return int;  -- ..\SDL2_tmp\SDL_cpuinfo.h:100
   pragma Import (C, SDL_GetCPUCount, "SDL_GetCPUCount");

  --*
  -- *  This function returns the L1 cache line size of the CPU
  -- *
  -- *  This is useful for determining multi-threaded structure padding
  -- *  or SIMD prefetch sizes.
  --  

   function SDL_GetCPUCacheLineSize return int;  -- ..\SDL2_tmp\SDL_cpuinfo.h:108
   pragma Import (C, SDL_GetCPUCacheLineSize, "SDL_GetCPUCacheLineSize");

  --*
  -- *  This function returns true if the CPU has the RDTSC instruction.
  --  

   function SDL_HasRDTSC return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_cpuinfo.h:113
   pragma Import (C, SDL_HasRDTSC, "SDL_HasRDTSC");

  --*
  -- *  This function returns true if the CPU has AltiVec features.
  --  

   function SDL_HasAltiVec return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_cpuinfo.h:118
   pragma Import (C, SDL_HasAltiVec, "SDL_HasAltiVec");

  --*
  -- *  This function returns true if the CPU has MMX features.
  --  

   function SDL_HasMMX return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_cpuinfo.h:123
   pragma Import (C, SDL_HasMMX, "SDL_HasMMX");

  --*
  -- *  This function returns true if the CPU has 3DNow! features.
  --  

   function SDL_Has3DNow return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_cpuinfo.h:128
   pragma Import (C, SDL_Has3DNow, "SDL_Has3DNow");

  --*
  -- *  This function returns true if the CPU has SSE features.
  --  

   function SDL_HasSSE return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_cpuinfo.h:133
   pragma Import (C, SDL_HasSSE, "SDL_HasSSE");

  --*
  -- *  This function returns true if the CPU has SSE2 features.
  --  

   function SDL_HasSSE2 return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_cpuinfo.h:138
   pragma Import (C, SDL_HasSSE2, "SDL_HasSSE2");

  --*
  -- *  This function returns true if the CPU has SSE3 features.
  --  

   function SDL_HasSSE3 return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_cpuinfo.h:143
   pragma Import (C, SDL_HasSSE3, "SDL_HasSSE3");

  --*
  -- *  This function returns true if the CPU has SSE4.1 features.
  --  

   function SDL_HasSSE41 return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_cpuinfo.h:148
   pragma Import (C, SDL_HasSSE41, "SDL_HasSSE41");

  --*
  -- *  This function returns true if the CPU has SSE4.2 features.
  --  

   function SDL_HasSSE42 return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_cpuinfo.h:153
   pragma Import (C, SDL_HasSSE42, "SDL_HasSSE42");

  --*
  -- *  This function returns true if the CPU has AVX features.
  --  

   function SDL_HasAVX return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_cpuinfo.h:158
   pragma Import (C, SDL_HasAVX, "SDL_HasAVX");

  --*
  -- *  This function returns true if the CPU has AVX2 features.
  --  

   function SDL_HasAVX2 return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_cpuinfo.h:163
   pragma Import (C, SDL_HasAVX2, "SDL_HasAVX2");

  --*
  -- *  This function returns true if the CPU has AVX-512F (foundation) features.
  --  

   function SDL_HasAVX512F return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_cpuinfo.h:168
   pragma Import (C, SDL_HasAVX512F, "SDL_HasAVX512F");

  --*
  -- *  This function returns true if the CPU has NEON (ARM SIMD) features.
  --  

   function SDL_HasNEON return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_cpuinfo.h:173
   pragma Import (C, SDL_HasNEON, "SDL_HasNEON");

  --*
  -- *  This function returns the amount of RAM configured in the system, in MB.
  --  

   function SDL_GetSystemRAM return int;  -- ..\SDL2_tmp\SDL_cpuinfo.h:178
   pragma Import (C, SDL_GetSystemRAM, "SDL_GetSystemRAM");

  -- Ends C function definitions when using C++  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_cpuinfo_h;
