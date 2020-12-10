pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

package SDL_platform_h is

   HAVE_WINAPIFAMILY_H : constant := 0;  --  ..\SDL2_tmp\SDL_platform.h:134

   WINAPI_FAMILY_WINRT : constant := 0;  --  ..\SDL2_tmp\SDL_platform.h:141

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
  -- *  \file SDL_platform.h
  -- *
  -- *  Try to get a standard set of platform defines.
  --  

  -- lets us know what version of Mac OS X we're compiling on  
  -- if compiling for iOS  
  -- if not compiling for iOS  
  -- Try to find out if we're compiling for WinRT or non-WinRT  
  -- If _USING_V110_SDK71_ is defined it means we are using the Windows XP toolset.  
  -- The NACL compiler defines __native_client__ and __pnacl__
  -- * Ref: http://www.chromium.org/nativeclient/pnacl/stability-of-the-pnacl-bitcode-abi
  --  

  -- PNACL with newlib supports static linking only  
  -- Set up for C function definitions, even when using C++  
  --*
  -- *  \brief Gets the name of the platform.
  --  

   function SDL_GetPlatform return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_platform.h:188
   pragma Import (C, SDL_GetPlatform, "SDL_GetPlatform");

  -- Ends C function definitions when using C++  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_platform_h;
