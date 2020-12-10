pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with SDL_SDL_stdinc_h;

package SDL_SDL_thread_h is

   --  skipped empty struct SDL_Thread

   function SDL_CreateThread (fn : access function (arg1 : System.Address) return int; data : System.Address) return System.Address;  -- ../include/SDL/SDL_thread.h:88
   pragma Import (C, SDL_CreateThread, "SDL_CreateThread");

   function SDL_ThreadID return SDL_SDL_stdinc_h.Uint32;  -- ../include/SDL/SDL_thread.h:92
   pragma Import (C, SDL_ThreadID, "SDL_ThreadID");

   function SDL_GetThreadID (thread : System.Address) return SDL_SDL_stdinc_h.Uint32;  -- ../include/SDL/SDL_thread.h:97
   pragma Import (C, SDL_GetThreadID, "SDL_GetThreadID");

   procedure SDL_WaitThread (thread : System.Address; status : access int);  -- ../include/SDL/SDL_thread.h:103
   pragma Import (C, SDL_WaitThread, "SDL_WaitThread");

   procedure SDL_KillThread (thread : System.Address);  -- ../include/SDL/SDL_thread.h:106
   pragma Import (C, SDL_KillThread, "SDL_KillThread");

end SDL_SDL_thread_h;
