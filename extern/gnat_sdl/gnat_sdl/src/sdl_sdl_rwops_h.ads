pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
limited with stdio_h;
with SDL_SDL_stdinc_h;
with Interfaces.C.Strings;

package SDL_SDL_rwops_h is


   RW_SEEK_SET : constant := 0;  --  ../include/SDL/SDL_rwops.h:115
   RW_SEEK_CUR : constant := 1;  --  ../include/SDL/SDL_rwops.h:116
   RW_SEEK_END : constant := 2;  --  ../include/SDL/SDL_rwops.h:117
   --  arg-macro: function SDL_RWseek (ctx, offset, wh(ctx).seek(ctx, offset, whence
   --    return ctx).seek(ctx, offset, whence;
   --  arg-macro: function SDL_RWtell (ctx)
   --    return ctx).seek(ctx, 0, RW_SEEK_CUR;
   --  arg-macro: function SDL_RWread (ctx, ptr, size,(ctx).read(ctx, ptr, size, n
   --    return ctx).read(ctx, ptr, size, n;
   --  arg-macro: function SDL_RWwrite (ctx, ptr, size,(ctx).write(ctx, ptr, size, n
   --    return ctx).write(ctx, ptr, size, n;
   --  arg-macro: function SDL_RWclose (ctx)
   --    return ctx).close(ctx;

   type anon_10;
   type anon_11;
   type anon_12 is record
      data : System.Address;  -- ../include/SDL/SDL_rwops.h:71
      size : aliased int;  -- ../include/SDL/SDL_rwops.h:72
      left : aliased int;  -- ../include/SDL/SDL_rwops.h:73
   end record;
   pragma Convention (C_Pass_By_Copy, anon_12);
   type anon_11 is record
      append : aliased int;  -- ../include/SDL/SDL_rwops.h:68
      h : System.Address;  -- ../include/SDL/SDL_rwops.h:69
      buffer : aliased anon_12;  -- ../include/SDL/SDL_rwops.h:74
   end record;
   pragma Convention (C_Pass_By_Copy, anon_11);
   type anon_13 is record
      autoclose : aliased int;  -- ../include/SDL/SDL_rwops.h:79
      fp : System.Address;  -- ../include/SDL/SDL_rwops.h:80
   end record;
   pragma Convention (C_Pass_By_Copy, anon_13);
   type anon_14 is record
      base : access SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_rwops.h:84
      here : access SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_rwops.h:85
      stop : access SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_rwops.h:86
   end record;
   pragma Convention (C_Pass_By_Copy, anon_14);
   type anon_15 is record
      data1 : System.Address;  -- ../include/SDL/SDL_rwops.h:89
   end record;
   pragma Convention (C_Pass_By_Copy, anon_15);
   type anon_10 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            win32io : aliased anon_11;  -- ../include/SDL/SDL_rwops.h:75
         when 1 =>
            stdio : aliased anon_13;  -- ../include/SDL/SDL_rwops.h:81
         when 2 =>
            mem : aliased anon_14;  -- ../include/SDL/SDL_rwops.h:87
         when others =>
            unknown : aliased anon_15;  -- ../include/SDL/SDL_rwops.h:90
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, anon_10);
   pragma Unchecked_Union (anon_10);
   type SDL_RWops is record
      seek : access function
           (arg1 : access SDL_RWops;
            arg2 : int;
            arg3 : int) return int;  -- ../include/SDL/SDL_rwops.h:47
      read : access function
           (arg1 : access SDL_RWops;
            arg2 : System.Address;
            arg3 : int;
            arg4 : int) return int;  -- ../include/SDL/SDL_rwops.h:53
      write : access function
           (arg1 : access SDL_RWops;
            arg2 : System.Address;
            arg3 : int;
            arg4 : int) return int;  -- ../include/SDL/SDL_rwops.h:59
      close : access function (arg1 : access SDL_RWops) return int;  -- ../include/SDL/SDL_rwops.h:62
      c_type : aliased SDL_SDL_stdinc_h.Uint32;  -- ../include/SDL/SDL_rwops.h:64
      hidden : anon_10;  -- ../include/SDL/SDL_rwops.h:91
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_RWops);  -- ../include/SDL/SDL_rwops.h:42

   function SDL_RWFromFile (file : Interfaces.C.Strings.chars_ptr; mode : Interfaces.C.Strings.chars_ptr) return access SDL_RWops;  -- ../include/SDL/SDL_rwops.h:99
   pragma Import (C, SDL_RWFromFile, "SDL_RWFromFile");

   function SDL_RWFromFP (fp : System.Address; autoclose : int) return access SDL_RWops;  -- ../include/SDL/SDL_rwops.h:102
   pragma Import (C, SDL_RWFromFP, "SDL_RWFromFP");

   function SDL_RWFromMem (mem : System.Address; size : int) return access SDL_RWops;  -- ../include/SDL/SDL_rwops.h:105
   pragma Import (C, SDL_RWFromMem, "SDL_RWFromMem");

   function SDL_RWFromConstMem (mem : System.Address; size : int) return access SDL_RWops;  -- ../include/SDL/SDL_rwops.h:106
   pragma Import (C, SDL_RWFromConstMem, "SDL_RWFromConstMem");

   function SDL_AllocRW return access SDL_RWops;  -- ../include/SDL/SDL_rwops.h:108
   pragma Import (C, SDL_AllocRW, "SDL_AllocRW");

   procedure SDL_FreeRW (area : access SDL_RWops);  -- ../include/SDL/SDL_rwops.h:109
   pragma Import (C, SDL_FreeRW, "SDL_FreeRW");

   function SDL_ReadLE16 (src : access SDL_RWops) return SDL_SDL_stdinc_h.Uint16;  -- ../include/SDL/SDL_rwops.h:131
   pragma Import (C, SDL_ReadLE16, "SDL_ReadLE16");

   function SDL_ReadBE16 (src : access SDL_RWops) return SDL_SDL_stdinc_h.Uint16;  -- ../include/SDL/SDL_rwops.h:132
   pragma Import (C, SDL_ReadBE16, "SDL_ReadBE16");

   function SDL_ReadLE32 (src : access SDL_RWops) return SDL_SDL_stdinc_h.Uint32;  -- ../include/SDL/SDL_rwops.h:133
   pragma Import (C, SDL_ReadLE32, "SDL_ReadLE32");

   function SDL_ReadBE32 (src : access SDL_RWops) return SDL_SDL_stdinc_h.Uint32;  -- ../include/SDL/SDL_rwops.h:134
   pragma Import (C, SDL_ReadBE32, "SDL_ReadBE32");

   function SDL_ReadLE64 (src : access SDL_RWops) return SDL_SDL_stdinc_h.Uint64;  -- ../include/SDL/SDL_rwops.h:135
   pragma Import (C, SDL_ReadLE64, "SDL_ReadLE64");

   function SDL_ReadBE64 (src : access SDL_RWops) return SDL_SDL_stdinc_h.Uint64;  -- ../include/SDL/SDL_rwops.h:136
   pragma Import (C, SDL_ReadBE64, "SDL_ReadBE64");

   function SDL_WriteLE16 (dst : access SDL_RWops; value : SDL_SDL_stdinc_h.Uint16) return int;  -- ../include/SDL/SDL_rwops.h:141
   pragma Import (C, SDL_WriteLE16, "SDL_WriteLE16");

   function SDL_WriteBE16 (dst : access SDL_RWops; value : SDL_SDL_stdinc_h.Uint16) return int;  -- ../include/SDL/SDL_rwops.h:142
   pragma Import (C, SDL_WriteBE16, "SDL_WriteBE16");

   function SDL_WriteLE32 (dst : access SDL_RWops; value : SDL_SDL_stdinc_h.Uint32) return int;  -- ../include/SDL/SDL_rwops.h:143
   pragma Import (C, SDL_WriteLE32, "SDL_WriteLE32");

   function SDL_WriteBE32 (dst : access SDL_RWops; value : SDL_SDL_stdinc_h.Uint32) return int;  -- ../include/SDL/SDL_rwops.h:144
   pragma Import (C, SDL_WriteBE32, "SDL_WriteBE32");

   function SDL_WriteLE64 (dst : access SDL_RWops; value : SDL_SDL_stdinc_h.Uint64) return int;  -- ../include/SDL/SDL_rwops.h:145
   pragma Import (C, SDL_WriteLE64, "SDL_WriteLE64");

   function SDL_WriteBE64 (dst : access SDL_RWops; value : SDL_SDL_stdinc_h.Uint64) return int;  -- ../include/SDL/SDL_rwops.h:146
   pragma Import (C, SDL_WriteBE64, "SDL_WriteBE64");

end SDL_SDL_rwops_h;
