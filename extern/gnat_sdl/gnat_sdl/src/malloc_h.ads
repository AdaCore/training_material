pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with umingw_h;

package malloc_h is

   --  arg-macro: procedure alloca (x)
   --    __builtin_alloca((x))
   type u_heapinfo is record
      u_pentry : access int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/malloc.h:47
      u_size : aliased umingw_h.size_t;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/malloc.h:48
      u_useflag : aliased int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/malloc.h:49
   end record;
   pragma Convention (C_Pass_By_Copy, u_heapinfo);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/malloc.h:46

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
