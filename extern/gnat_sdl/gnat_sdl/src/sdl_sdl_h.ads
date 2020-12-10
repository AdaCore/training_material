pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_SDL_stdinc_h;

package SDL_SDL_h is


   SDL_INIT_TIMER : constant := 16#00000001#;  --  ../include/SDL/SDL.h:61
   SDL_INIT_AUDIO : constant := 16#00000010#;  --  ../include/SDL/SDL.h:62
   SDL_INIT_VIDEO : constant := 16#00000020#;  --  ../include/SDL/SDL.h:63
   SDL_INIT_CDROM : constant := 16#00000100#;  --  ../include/SDL/SDL.h:64
   SDL_INIT_JOYSTICK : constant := 16#00000200#;  --  ../include/SDL/SDL.h:65
   SDL_INIT_NOPARACHUTE : constant := 16#00100000#;  --  ../include/SDL/SDL.h:66
   SDL_INIT_EVENTTHREAD : constant := 16#01000000#;  --  ../include/SDL/SDL.h:67
   SDL_INIT_EVERYTHING : constant := 16#0000FFFF#;  --  ../include/SDL/SDL.h:68

   function SDL_Init (flags : SDL_SDL_stdinc_h.Uint32) return int;  -- ../include/SDL/SDL.h:76
   pragma Import (C, SDL_Init, "SDL_Init");

   function SDL_InitSubSystem (flags : SDL_SDL_stdinc_h.Uint32) return int;  -- ../include/SDL/SDL.h:79
   pragma Import (C, SDL_InitSubSystem, "SDL_InitSubSystem");

   procedure SDL_QuitSubSystem (flags : SDL_SDL_stdinc_h.Uint32);  -- ../include/SDL/SDL.h:82
   pragma Import (C, SDL_QuitSubSystem, "SDL_QuitSubSystem");

   function SDL_WasInit (flags : SDL_SDL_stdinc_h.Uint32) return SDL_SDL_stdinc_h.Uint32;  -- ../include/SDL/SDL.h:88
   pragma Import (C, SDL_WasInit, "SDL_WasInit");

   procedure SDL_Quit;  -- ../include/SDL/SDL.h:93
   pragma Import (C, SDL_Quit, "SDL_Quit");

end SDL_SDL_h;
