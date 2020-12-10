pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with SDL_stdinc_h;

package SDL_clipboard_h is

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
  -- * \file SDL_clipboard.h
  -- *
  -- * Include file for SDL clipboard handling
  --  

  -- Set up for C function definitions, even when using C++  
  -- Function prototypes  
  --*
  -- * \brief Put UTF-8 text into the clipboard
  -- *
  -- * \sa SDL_GetClipboardText()
  --  

   function SDL_SetClipboardText (text : Interfaces.C.Strings.chars_ptr) return int;  -- ..\SDL2_tmp\SDL_clipboard.h:46
   pragma Import (C, SDL_SetClipboardText, "SDL_SetClipboardText");

  --*
  -- * \brief Get UTF-8 text from the clipboard, which must be freed with SDL_free()
  -- *
  -- * \sa SDL_SetClipboardText()
  --  

   function SDL_GetClipboardText return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_clipboard.h:53
   pragma Import (C, SDL_GetClipboardText, "SDL_GetClipboardText");

  --*
  -- * \brief Returns a flag indicating whether the clipboard exists and contains a text string that is non-empty
  -- *
  -- * \sa SDL_GetClipboardText()
  --  

   function SDL_HasClipboardText return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_clipboard.h:60
   pragma Import (C, SDL_HasClipboardText, "SDL_HasClipboardText");

  -- Ends C function definitions when using C++  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_clipboard_h;
