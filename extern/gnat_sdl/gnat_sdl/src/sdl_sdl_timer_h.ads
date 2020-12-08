pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_SDL_stdinc_h;
with System;

package SDL_SDL_timer_h is


   SDL_TIMESLICE : constant := 10;  --  ../include/SDL/SDL_timer.h:40

   TIMER_RESOLUTION : constant := 10;  --  ../include/SDL/SDL_timer.h:43

   function SDL_GetTicks return SDL_SDL_stdinc_h.Uint32;  -- ../include/SDL/SDL_timer.h:49
   pragma Import (C, SDL_GetTicks, "SDL_GetTicks");

   procedure SDL_Delay (ms : SDL_SDL_stdinc_h.Uint32);  -- ../include/SDL/SDL_timer.h:52
   pragma Import (C, SDL_Delay, "SDL_Delay");

   type SDL_TimerCallback is access function (arg1 : SDL_SDL_stdinc_h.Uint32) return SDL_SDL_stdinc_h.Uint32;
   pragma Convention (C, SDL_TimerCallback);  -- ../include/SDL/SDL_timer.h:55

   function SDL_SetTimer (interval : SDL_SDL_stdinc_h.Uint32; callback : SDL_TimerCallback) return int;  -- ../include/SDL/SDL_timer.h:86
   pragma Import (C, SDL_SetTimer, "SDL_SetTimer");

   type SDL_NewTimerCallback is access function (arg1 : SDL_SDL_stdinc_h.Uint32; arg2 : System.Address) return SDL_SDL_stdinc_h.Uint32;
   pragma Convention (C, SDL_NewTimerCallback);  -- ../include/SDL/SDL_timer.h:101

   --  skipped empty struct u_SDL_TimerID

   type SDL_TimerID is new System.Address;  -- ../include/SDL/SDL_timer.h:104

   function SDL_AddTimer
     (interval : SDL_SDL_stdinc_h.Uint32;
      callback : SDL_NewTimerCallback;
      param : System.Address) return SDL_TimerID;  -- ../include/SDL/SDL_timer.h:109
   pragma Import (C, SDL_AddTimer, "SDL_AddTimer");

   function SDL_RemoveTimer (t : SDL_TimerID) return SDL_SDL_stdinc_h.SDL_bool;  -- ../include/SDL/SDL_timer.h:115
   pragma Import (C, SDL_RemoveTimer, "SDL_RemoveTimer");

end SDL_SDL_timer_h;
