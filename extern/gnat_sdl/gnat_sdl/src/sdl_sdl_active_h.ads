pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_SDL_stdinc_h;

package SDL_SDL_active_h is


   SDL_APPMOUSEFOCUS : constant := 16#01#;  --  ../include/SDL/SDL_active.h:42
   SDL_APPINPUTFOCUS : constant := 16#02#;  --  ../include/SDL/SDL_active.h:43
   SDL_APPACTIVE : constant := 16#04#;  --  ../include/SDL/SDL_active.h:44

   function SDL_GetAppState return SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_active.h:54
   pragma Import (C, SDL_GetAppState, "SDL_GetAppState");

end SDL_SDL_active_h;
