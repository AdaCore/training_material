pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package SDL_power_h is

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
  -- *  \file SDL_power.h
  -- *
  -- *  Header for the SDL power management routines.
  --  

  -- Set up for C function definitions, even when using C++  
  --*
  -- *  \brief The basic state for the system's power supply.
  --  

  --*< cannot determine power status  
  --*< Not plugged in, running on the battery  
  --*< Plugged in, no battery available  
  --*< Plugged in, charging battery  
  --*< Plugged in, battery charged  
   type SDL_PowerState is 
     (SDL_POWERSTATE_UNKNOWN,
      SDL_POWERSTATE_ON_BATTERY,
      SDL_POWERSTATE_NO_BATTERY,
      SDL_POWERSTATE_CHARGING,
      SDL_POWERSTATE_CHARGED);
   pragma Convention (C, SDL_PowerState);  -- ..\SDL2_tmp\SDL_power.h:49

  --*
  -- *  \brief Get the current power supply details.
  -- *
  -- *  \param secs Seconds of battery life left. You can pass a NULL here if
  -- *              you don't care. Will return -1 if we can't determine a
  -- *              value, or we're not running on a battery.
  -- *
  -- *  \param pct Percentage of battery life left, between 0 and 100. You can
  -- *             pass a NULL here if you don't care. Will return -1 if we
  -- *             can't determine a value, or we're not running on a battery.
  -- *
  -- *  \return The state of the battery (if any).
  --  

   function SDL_GetPowerInfo (secs : access int; pct : access int) return SDL_PowerState;  -- ..\SDL2_tmp\SDL_power.h:65
   pragma Import (C, SDL_GetPowerInfo, "SDL_GetPowerInfo");

  -- Ends C function definitions when using C++  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_power_h;
