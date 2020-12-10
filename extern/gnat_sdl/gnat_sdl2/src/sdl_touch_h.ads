pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_stdinc_h;

package SDL_touch_h is

   --  unsupported macro: SDL_TOUCH_MOUSEID ((Uint32)-1)
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
  -- *  \file SDL_touch.h
  -- *
  -- *  Include file for SDL touch event handling.
  --  

  -- Set up for C function definitions, even when using C++  
   subtype SDL_TouchID is SDL_stdinc_h.Sint64;  -- ..\SDL2_tmp\SDL_touch.h:41

   subtype SDL_FingerID is SDL_stdinc_h.Sint64;  -- ..\SDL2_tmp\SDL_touch.h:42

   type SDL_Finger is record
      id : aliased SDL_FingerID;  -- ..\SDL2_tmp\SDL_touch.h:46
      x : aliased float;  -- ..\SDL2_tmp\SDL_touch.h:47
      y : aliased float;  -- ..\SDL2_tmp\SDL_touch.h:48
      pressure : aliased float;  -- ..\SDL2_tmp\SDL_touch.h:49
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_Finger);  -- ..\SDL2_tmp\SDL_touch.h:44

  -- Used as the device ID for mouse events simulated with touch input  
  -- Function prototypes  
  --*
  -- *  \brief Get the number of registered touch devices.
  --  

   function SDL_GetNumTouchDevices return int;  -- ..\SDL2_tmp\SDL_touch.h:61
   pragma Import (C, SDL_GetNumTouchDevices, "SDL_GetNumTouchDevices");

  --*
  -- *  \brief Get the touch ID with the given index, or 0 if the index is invalid.
  --  

   function SDL_GetTouchDevice (index : int) return SDL_TouchID;  -- ..\SDL2_tmp\SDL_touch.h:66
   pragma Import (C, SDL_GetTouchDevice, "SDL_GetTouchDevice");

  --*
  -- *  \brief Get the number of active fingers for a given touch device.
  --  

   function SDL_GetNumTouchFingers (touchID : SDL_TouchID) return int;  -- ..\SDL2_tmp\SDL_touch.h:71
   pragma Import (C, SDL_GetNumTouchFingers, "SDL_GetNumTouchFingers");

  --*
  -- *  \brief Get the finger object of the given touch, with the given index.
  --  

   function SDL_GetTouchFinger (touchID : SDL_TouchID; index : int) return access SDL_Finger;  -- ..\SDL2_tmp\SDL_touch.h:76
   pragma Import (C, SDL_GetTouchFinger, "SDL_GetTouchFinger");

  -- Ends C function definitions when using C++  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_touch_h;
