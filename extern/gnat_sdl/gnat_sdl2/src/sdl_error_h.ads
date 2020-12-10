pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

package SDL_error_h is

   --  arg-macro: procedure SDL_OutOfMemory ()
   --    SDL_Error(SDL_ENOMEM)
   --  arg-macro: procedure SDL_Unsupported ()
   --    SDL_Error(SDL_UNSUPPORTED)
   --  arg-macro: procedure SDL_InvalidParamError (param)
   --    SDL_SetError("Parameter '%s' is invalid", (param))
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
  -- *  \file SDL_error.h
  -- *
  -- *  Simple error message routines for SDL.
  --  

  -- Set up for C function definitions, even when using C++  
  -- Public functions  
  -- SDL_SetError() unconditionally returns -1.  
   function SDL_SetError (fmt : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- ..\SDL2_tmp\SDL_error.h:41
   pragma Import (C, SDL_SetError, "SDL_SetError");

   function SDL_GetError return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_error.h:42
   pragma Import (C, SDL_GetError, "SDL_GetError");

   procedure SDL_ClearError;  -- ..\SDL2_tmp\SDL_error.h:43
   pragma Import (C, SDL_ClearError, "SDL_ClearError");

  --*
  -- *  \name Internal error functions
  -- *
  -- *  \internal
  -- *  Private error reporting function - used internally.
  --  

  -- @{  
   type SDL_errorcode is 
     (SDL_ENOMEM,
      SDL_EFREAD,
      SDL_EFWRITE,
      SDL_EFSEEK,
      SDL_UNSUPPORTED,
      SDL_LASTERROR);
   pragma Convention (C, SDL_errorcode);  -- ..\SDL2_tmp\SDL_error.h:63

  -- SDL_Error() unconditionally returns -1.  
   function SDL_Error (code : SDL_errorcode) return int;  -- ..\SDL2_tmp\SDL_error.h:65
   pragma Import (C, SDL_Error, "SDL_Error");

  -- @}  
  -- Internal error functions  
  -- Ends C function definitions when using C++  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_error_h;
