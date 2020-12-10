pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Strings;
with SDL_SDL_stdinc_h;

package SDL_SDL_main_h is


   C_LINKAGE : aliased constant String := "C" & ASCII.NUL;  --  ../include/SDL/SDL_main.h:38
   --  unsupported macro: main SDL_main

   function SDL_main (argc : int; argv : System.Address) return int;  -- ../include/SDL/SDL_main.h:57
   pragma Import (C, SDL_main, "SDL_main");

   procedure SDL_SetModuleHandle (hInst : System.Address);  -- ../include/SDL/SDL_main.h:70
   pragma Import (C, SDL_SetModuleHandle, "SDL_SetModuleHandle");

   function SDL_RegisterApp
     (name : Interfaces.C.Strings.chars_ptr;
      style : SDL_SDL_stdinc_h.Uint32;
      hInst : System.Address) return int;  -- ../include/SDL/SDL_main.h:72
   pragma Import (C, SDL_RegisterApp, "SDL_RegisterApp");

   procedure SDL_UnregisterApp;  -- ../include/SDL/SDL_main.h:74
   pragma Import (C, SDL_UnregisterApp, "SDL_UnregisterApp");

end SDL_SDL_main_h;
