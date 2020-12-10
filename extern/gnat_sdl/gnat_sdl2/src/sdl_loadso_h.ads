pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;

package SDL_loadso_h is

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
  -- *  \file SDL_loadso.h
  -- *
  -- *  System dependent library loading routines
  -- *
  -- *  Some things to keep in mind:
  -- *  \li These functions only work on C function names.  Other languages may
  -- *      have name mangling and intrinsic language support that varies from
  -- *      compiler to compiler.
  -- *  \li Make sure you declare your function pointers with the same calling
  -- *      convention as the actual library function.  Your code will crash
  -- *      mysteriously if you do not do this.
  -- *  \li Avoid namespace collisions.  If you load a symbol from the library,
  -- *      it is not defined whether or not it goes into the global symbol
  -- *      namespace for the application.  If it does and it conflicts with
  -- *      symbols in your code or other shared libraries, you will not get
  -- *      the results you expect. :)
  --  

  -- Set up for C function definitions, even when using C++  
  --*
  -- *  This function dynamically loads a shared object and returns a pointer
  -- *  to the object handle (or NULL if there was an error).
  -- *  The 'sofile' parameter is a system dependent name of the object file.
  --  

   function SDL_LoadObject (sofile : Interfaces.C.Strings.chars_ptr) return System.Address;  -- ..\SDL2_tmp\SDL_loadso.h:58
   pragma Import (C, SDL_LoadObject, "SDL_LoadObject");

  --*
  -- *  Given an object handle, this function looks up the address of the
  -- *  named function in the shared object and returns it.  This address
  -- *  is no longer valid after calling SDL_UnloadObject().
  --  

   function SDL_LoadFunction (handle : System.Address; name : Interfaces.C.Strings.chars_ptr) return System.Address;  -- ..\SDL2_tmp\SDL_loadso.h:65
   pragma Import (C, SDL_LoadFunction, "SDL_LoadFunction");

  --*
  -- *  Unload a shared object from memory.
  --  

   procedure SDL_UnloadObject (handle : System.Address);  -- ..\SDL2_tmp\SDL_loadso.h:71
   pragma Import (C, SDL_UnloadObject, "SDL_UnloadObject");

  -- Ends C function definitions when using C++  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_loadso_h;
