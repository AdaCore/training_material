pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;

package SDL_SDL_loadso_h is

   function SDL_LoadObject (sofile : Interfaces.C.Strings.chars_ptr) return System.Address;  -- ../include/SDL/SDL_loadso.h:60
   pragma Import (C, SDL_LoadObject, "SDL_LoadObject");

   function SDL_LoadFunction (handle : System.Address; name : Interfaces.C.Strings.chars_ptr) return System.Address;  -- ../include/SDL/SDL_loadso.h:67
   pragma Import (C, SDL_LoadFunction, "SDL_LoadFunction");

   procedure SDL_UnloadObject (handle : System.Address);  -- ../include/SDL/SDL_loadso.h:70
   pragma Import (C, SDL_UnloadObject, "SDL_UnloadObject");

end SDL_SDL_loadso_h;
