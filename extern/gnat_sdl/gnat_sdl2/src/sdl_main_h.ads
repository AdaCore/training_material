pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Strings;
with SDL_stdinc_h;

package SDL_main_h is

   C_LINKAGE : aliased constant String := "C" & ASCII.NUL;  --  ..\SDL2_tmp\SDL_main.h:86
   --  unsupported macro: main SDL_main

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
  -- *  \file SDL_main.h
  -- *
  -- *  Redefine main() on some platforms so that it is called by SDL.
  --  

  -- On Windows SDL provides WinMain(), which parses the command line and passes
  --   the arguments to your main function.
  --   If you provide your own WinMain(), you may define SDL_MAIN_HANDLED
  --  

  -- On WinRT, SDL provides a main function that initializes CoreApplication,
  --   creating an instance of IFrameworkView in the process.
  --   Please note that #include'ing SDL_main.h is not enough to get a main()
  --   function working.  In non-XAML apps, the file,
  --   src/main/winrt/SDL_WinRT_main_NonXAML.cpp, or a copy of it, must be compiled
  --   into the app itself.  In XAML apps, the function, SDL_WinRTRunApp must be
  --   called, with a pointer to the Direct3D-hosted XAML control passed in.
  -- 

  -- On iOS SDL provides a main function that creates an application delegate
  --   and starts the iOS application run loop.
  --   See src/video/uikit/SDL_uikitappdelegate.m for more details.
  --  

  -- On Android SDL provides a Java class in SDLActivity.java that is the
  --   main activity entry point.
  --   See docs/README-android.md for more details on extending that class.
  --  

  -- We need to export SDL_main so it can be launched from Java  
  -- On NACL we use ppapi_simple to set up the application helper code,
  --   then wait for the first PSE_INSTANCE_DIDCHANGEVIEW event before 
  --   starting the user main function.
  --   All user code is run in a separate thread by ppapi_simple, thus 
  --   allowing for blocking io to take place via nacl_io
  -- 

  --*
  -- *  \file SDL_main.h
  -- *
  -- *  The application's main() function must be called with C linkage,
  -- *  and should be declared like this:
  -- *  \code
  -- *  #ifdef __cplusplus
  -- *  extern "C"
  -- *  #endif
  -- *  int main(int argc, char *argv[])
  -- *  {
  -- *  }
  -- *  \endcode
  --  

  --*
  -- *  The prototype for the application's main() function
  --  

   function SDL_main (argc : int; argv : System.Address) return int;  -- ..\SDL2_tmp\SDL_main.h:117
   pragma Import (C, SDL_main, "SDL_main");

  --*
  -- *  This is called by the real SDL main function to let the rest of the
  -- *  library know that initialization was done properly.
  -- *
  -- *  Calling this yourself without knowing what you're doing can cause
  -- *  crashes and hard to diagnose problems with your application.
  --  

   procedure SDL_SetMainReady;  -- ..\SDL2_tmp\SDL_main.h:132
   pragma Import (C, SDL_SetMainReady, "SDL_SetMainReady");

  --*
  -- *  This can be called to set the application class at startup
  --  

   function SDL_RegisterApp
     (name : Interfaces.C.Strings.chars_ptr;
      style : SDL_stdinc_h.Uint32;
      hInst : System.Address) return int;  -- ..\SDL2_tmp\SDL_main.h:139
   pragma Import (C, SDL_RegisterApp, "SDL_RegisterApp");

   procedure SDL_UnregisterApp;  -- ..\SDL2_tmp\SDL_main.h:141
   pragma Import (C, SDL_UnregisterApp, "SDL_UnregisterApp");

  --*
  -- *  \brief Initializes and launches an SDL/WinRT application.
  -- *
  -- *  \param mainFunction The SDL app's C-style main().
  -- *  \param reserved Reserved for future use; should be NULL
  -- *  \return 0 on success, -1 on failure.  On failure, use SDL_GetError to retrieve more
  -- *      information on the failure.
  --  

  -- vi: set ts=4 sw=4 expandtab:  
end SDL_main_h;
