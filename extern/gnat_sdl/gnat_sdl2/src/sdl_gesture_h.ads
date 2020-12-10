pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_stdinc_h;
with SDL_touch_h;
limited with SDL_rwops_h;

package SDL_gesture_h is

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
  -- *  \file SDL_gesture.h
  -- *
  -- *  Include file for SDL gesture event handling.
  --  

  -- Set up for C function definitions, even when using C++  
   subtype SDL_GestureID is SDL_stdinc_h.Sint64;  -- ..\SDL2_tmp\SDL_gesture.h:44

  -- Function prototypes  
  --*
  -- *  \brief Begin Recording a gesture on the specified touch, or all touches (-1)
  -- *
  -- *
  --  

   function SDL_RecordGesture (touchId : SDL_touch_h.SDL_TouchID) return int;  -- ..\SDL2_tmp\SDL_gesture.h:53
   pragma Import (C, SDL_RecordGesture, "SDL_RecordGesture");

  --*
  -- *  \brief Save all currently loaded Dollar Gesture templates
  -- *
  -- *
  --  

   function SDL_SaveAllDollarTemplates (dst : access SDL_rwops_h.SDL_RWops) return int;  -- ..\SDL2_tmp\SDL_gesture.h:61
   pragma Import (C, SDL_SaveAllDollarTemplates, "SDL_SaveAllDollarTemplates");

  --*
  -- *  \brief Save a currently loaded Dollar Gesture template
  -- *
  -- *
  --  

   function SDL_SaveDollarTemplate (gestureId : SDL_GestureID; dst : access SDL_rwops_h.SDL_RWops) return int;  -- ..\SDL2_tmp\SDL_gesture.h:68
   pragma Import (C, SDL_SaveDollarTemplate, "SDL_SaveDollarTemplate");

  --*
  -- *  \brief Load Dollar Gesture templates from a file
  -- *
  -- *
  --  

   function SDL_LoadDollarTemplates (touchId : SDL_touch_h.SDL_TouchID; src : access SDL_rwops_h.SDL_RWops) return int;  -- ..\SDL2_tmp\SDL_gesture.h:76
   pragma Import (C, SDL_LoadDollarTemplates, "SDL_LoadDollarTemplates");

  -- Ends C function definitions when using C++  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_gesture_h;
