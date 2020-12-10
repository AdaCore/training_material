pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_stdinc_h;

package SDL_h is

   SDL_INIT_TIMER : constant := 16#00000001#;  --  ..\SDL2_tmp\SDL.h:77
   SDL_INIT_AUDIO : constant := 16#00000010#;  --  ..\SDL2_tmp\SDL.h:78
   SDL_INIT_VIDEO : constant := 16#00000020#;  --  ..\SDL2_tmp\SDL.h:79
   SDL_INIT_JOYSTICK : constant := 16#00000200#;  --  ..\SDL2_tmp\SDL.h:80
   SDL_INIT_HAPTIC : constant := 16#00001000#;  --  ..\SDL2_tmp\SDL.h:81
   SDL_INIT_GAMECONTROLLER : constant := 16#00002000#;  --  ..\SDL2_tmp\SDL.h:82
   SDL_INIT_EVENTS : constant := 16#00004000#;  --  ..\SDL2_tmp\SDL.h:83
   SDL_INIT_SENSOR : constant := 16#00008000#;  --  ..\SDL2_tmp\SDL.h:84
   SDL_INIT_NOPARACHUTE : constant := 16#00100000#;  --  ..\SDL2_tmp\SDL.h:85
   --  unsupported macro: SDL_INIT_EVERYTHING ( SDL_INIT_TIMER | SDL_INIT_AUDIO | SDL_INIT_VIDEO | SDL_INIT_EVENTS | SDL_INIT_JOYSTICK | SDL_INIT_HAPTIC | SDL_INIT_GAMECONTROLLER | SDL_INIT_SENSOR )

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
  -- *  \file SDL.h
  -- *
  -- *  Main include header for the SDL library
  --  

  -- Set up for C function definitions, even when using C++  
  -- As of version 0.5, SDL is loaded dynamically into the application  
  --*
  -- *  \name SDL_INIT_*
  -- *
  -- *  These are the flags which may be passed to SDL_Init().  You should
  -- *  specify the subsystems which you will be using in your application.
  --  

  -- @{  
  -- @}  
  --*
  -- *  This function initializes  the subsystems specified by \c flags
  --  

   function SDL_Init (flags : SDL_stdinc_h.Uint32) return int;  -- ..\SDL2_tmp\SDL.h:95
   pragma Import (C, SDL_Init, "SDL_Init");

  --*
  -- *  This function initializes specific SDL subsystems
  -- *
  -- *  Subsystem initialization is ref-counted, you must call
  -- *  SDL_QuitSubSystem() for each SDL_InitSubSystem() to correctly
  -- *  shutdown a subsystem manually (or call SDL_Quit() to force shutdown).
  -- *  If a subsystem is already loaded then this call will
  -- *  increase the ref-count and return.
  --  

   function SDL_InitSubSystem (flags : SDL_stdinc_h.Uint32) return int;  -- ..\SDL2_tmp\SDL.h:106
   pragma Import (C, SDL_InitSubSystem, "SDL_InitSubSystem");

  --*
  -- *  This function cleans up specific SDL subsystems
  --  

   procedure SDL_QuitSubSystem (flags : SDL_stdinc_h.Uint32);  -- ..\SDL2_tmp\SDL.h:111
   pragma Import (C, SDL_QuitSubSystem, "SDL_QuitSubSystem");

  --*
  -- *  This function returns a mask of the specified subsystems which have
  -- *  previously been initialized.
  -- *
  -- *  If \c flags is 0, it returns a mask of all initialized subsystems.
  --  

   function SDL_WasInit (flags : SDL_stdinc_h.Uint32) return SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL.h:119
   pragma Import (C, SDL_WasInit, "SDL_WasInit");

  --*
  -- *  This function cleans up all initialized subsystems. You should
  -- *  call it upon all exit conditions.
  --  

   procedure SDL_Quit;  -- ..\SDL2_tmp\SDL.h:125
   pragma Import (C, SDL_Quit, "SDL_Quit");

  -- Ends C function definitions when using C++  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_h;
