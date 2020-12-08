pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_stdinc_h;
with System;

package SDL_timer_h is

   --  arg-macro: function SDL_TICKS_PASSED (A, B)
   --    return (Sint32)((B) - (A)) <= 0;
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
  -- *  \file SDL_timer.h
  -- *
  -- *  Header for the SDL time management routines.
  --  

  -- Set up for C function definitions, even when using C++  
  --*
  -- * \brief Get the number of milliseconds since the SDL library initialization.
  -- *
  -- * \note This value wraps if the program runs for more than ~49 days.
  --  

   function SDL_GetTicks return SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_timer.h:45
   pragma Import (C, SDL_GetTicks, "SDL_GetTicks");

  --*
  -- * \brief Compare SDL ticks values, and return true if A has passed B
  -- *
  -- * e.g. if you want to wait 100 ms, you could do this:
  -- *  Uint32 timeout = SDL_GetTicks() + 100;
  -- *  while (!SDL_TICKS_PASSED(SDL_GetTicks(), timeout)) {
  -- *      ... do work until timeout has elapsed
  -- *  }
  --  

  --*
  -- * \brief Get the current value of the high resolution counter
  --  

   function SDL_GetPerformanceCounter return SDL_stdinc_h.Uint64;  -- ..\SDL2_tmp\SDL_timer.h:61
   pragma Import (C, SDL_GetPerformanceCounter, "SDL_GetPerformanceCounter");

  --*
  -- * \brief Get the count per second of the high resolution counter
  --  

   function SDL_GetPerformanceFrequency return SDL_stdinc_h.Uint64;  -- ..\SDL2_tmp\SDL_timer.h:66
   pragma Import (C, SDL_GetPerformanceFrequency, "SDL_GetPerformanceFrequency");

  --*
  -- * \brief Wait a specified number of milliseconds before returning.
  --  

   procedure SDL_Delay (ms : SDL_stdinc_h.Uint32);  -- ..\SDL2_tmp\SDL_timer.h:71
   pragma Import (C, SDL_Delay, "SDL_Delay");

  --*
  -- *  Function prototype for the timer callback function.
  -- *
  -- *  The callback function is passed the current timer interval and returns
  -- *  the next timer interval.  If the returned value is the same as the one
  -- *  passed in, the periodic alarm continues, otherwise a new alarm is
  -- *  scheduled.  If the callback returns 0, the periodic alarm is cancelled.
  --  

   type SDL_TimerCallback is access function (arg1 : SDL_stdinc_h.Uint32; arg2 : System.Address) return SDL_stdinc_h.Uint32;
   pragma Convention (C, SDL_TimerCallback);  -- ..\SDL2_tmp\SDL_timer.h:81

  --*
  -- * Definition of the timer ID type.
  --  

   subtype SDL_TimerID is int;  -- ..\SDL2_tmp\SDL_timer.h:86

  --*
  -- * \brief Add a new timer to the pool of timers already running.
  -- *
  -- * \return A timer ID, or 0 when an error occurs.
  --  

   function SDL_AddTimer
     (interval : SDL_stdinc_h.Uint32;
      callback : SDL_TimerCallback;
      param : System.Address) return SDL_TimerID;  -- ..\SDL2_tmp\SDL_timer.h:93
   pragma Import (C, SDL_AddTimer, "SDL_AddTimer");

  --*
  -- * \brief Remove a timer knowing its ID.
  -- *
  -- * \return A boolean value indicating success or failure.
  -- *
  -- * \warning It is not safe to remove a timer multiple times.
  --  

   function SDL_RemoveTimer (id : SDL_TimerID) return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_timer.h:104
   pragma Import (C, SDL_RemoveTimer, "SDL_RemoveTimer");

  -- Ends C function definitions when using C++  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_timer_h;
