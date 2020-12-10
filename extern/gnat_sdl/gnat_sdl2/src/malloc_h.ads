pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with stddef_h;

package malloc_h is

   --  arg-macro: procedure alloca (x)
   --    __builtin_alloca((x))
  --*
  -- * This file has no copyright assigned and is placed in the Public Domain.
  -- * This file is part of the mingw-w64 runtime package.
  -- * No warranty is given; refer to the file DISCLAIMER.PD within this package.
  --  

  -- Return codes for _heapwalk()   
  -- Values for _heapinfo.useflag  
  -- The structure used to walk through the heap with _heapwalk.   
   type u_heapinfo is record
      u_pentry : access int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\malloc.h:47
      u_size : aliased stddef_h.size_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\malloc.h:48
      u_useflag : aliased int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\malloc.h:49
   end record;
   pragma Convention (C_Pass_By_Copy, u_heapinfo);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\malloc.h:46

  -- Make sure that X86intrin.h doesn't produce here collisions.   
  -- Users should really use MS provided versions  
   --  skipped func __mingw_aligned_malloc

   --  skipped func __mingw_aligned_free

   --  skipped func __mingw_aligned_offset_realloc

   --  skipped func __mingw_aligned_realloc

   --  skipped func _resetstkoflw

   --  skipped func _set_malloc_crt_max_wait

   --  skipped func _expand

   --  skipped func _msize

   --  skipped func _get_sbh_threshold

   --  skipped func _set_sbh_threshold

   --  skipped func _set_amblksiz

   --  skipped func _get_amblksiz

   --  skipped func _heapadd

   --  skipped func _heapchk

   --  skipped func _heapmin

   --  skipped func _heapset

   --  skipped func _heapwalk

   --  skipped func _heapused

   --  skipped func _get_heap_handle

   --  skipped func _MarkAllocaS

   --  skipped func _freea

end malloc_h;
