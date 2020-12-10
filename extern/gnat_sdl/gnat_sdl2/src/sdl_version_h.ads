pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_stdinc_h;
with Interfaces.C.Strings;

package SDL_version_h is

   SDL_MAJOR_VERSION : constant := 2;  --  ..\SDL2_tmp\SDL_version.h:60
   SDL_MINOR_VERSION : constant := 0;  --  ..\SDL2_tmp\SDL_version.h:61
   SDL_PATCHLEVEL : constant := 9;  --  ..\SDL2_tmp\SDL_version.h:62
   --  arg-macro: procedure SDL_VERSION (x)
   --    { (x).major := SDL_MAJOR_VERSION; (x).minor := SDL_MINOR_VERSION; (x).patch := SDL_PATCHLEVEL; }
   --  arg-macro: function SDL_VERSIONNUM (X, Y, Z)
   --    return (X)*1000 + (Y)*100 + (Z);
   --  unsupported macro: SDL_COMPILEDVERSION SDL_VERSIONNUM(SDL_MAJOR_VERSION, SDL_MINOR_VERSION, SDL_PATCHLEVEL)
   --  arg-macro: function SDL_VERSION_ATLEAST (X, Y, Z)
   --    return SDL_COMPILEDVERSION >= SDL_VERSIONNUM(X, Y, Z);

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
  -- *  \file SDL_version.h
  -- *
  -- *  This header defines the current SDL version.
  --  

  -- Set up for C function definitions, even when using C++  
  --*
  -- *  \brief Information the version of SDL in use.
  -- *
  -- *  Represents the library's version as three levels: major revision
  -- *  (increments with massive changes, additions, and enhancements),
  -- *  minor revision (increments with backwards-compatible changes to the
  -- *  major revision), and patchlevel (increments with fixes to the minor
  -- *  revision).
  -- *
  -- *  \sa SDL_VERSION
  -- *  \sa SDL_GetVersion
  --  

  --*< major version  
   type SDL_version is record
      major : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_version.h:53
      minor : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_version.h:54
      patch : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_version.h:55
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_version);  -- ..\SDL2_tmp\SDL_version.h:51

  --*< minor version  
  --*< update version  
  -- Printable format: "%d.%d.%d", MAJOR, MINOR, PATCHLEVEL
  -- 

  --*
  -- *  \brief Macro to determine SDL version program was compiled against.
  -- *
  -- *  This macro fills in a SDL_version structure with the version of the
  -- *  library you compiled against. This is determined by what header the
  -- *  compiler uses. Note that if you dynamically linked the library, you might
  -- *  have a slightly newer or older version at runtime. That version can be
  -- *  determined with SDL_GetVersion(), which, unlike SDL_VERSION(),
  -- *  is not a macro.
  -- *
  -- *  \param x A pointer to a SDL_version struct to initialize.
  -- *
  -- *  \sa SDL_version
  -- *  \sa SDL_GetVersion
  --  

  --*
  -- *  This macro turns the version numbers into a numeric value:
  -- *  \verbatim
  --    (1,2,3) -> (1203)
  --    \endverbatim
  -- *
  -- *  This assumes that there will never be more than 100 patchlevels.
  --  

  --*
  -- *  This is the version number macro for the current SDL version.
  --  

  --*
  -- *  This macro will evaluate to true if compiled with SDL at least X.Y.Z.
  --  

  --*
  -- *  \brief Get the version of SDL that is linked against your program.
  -- *
  -- *  If you are linking to SDL dynamically, then it is possible that the
  -- *  current version will be different than the version you compiled against.
  -- *  This function returns the current version, while SDL_VERSION() is a
  -- *  macro that tells you what version you compiled with.
  -- *
  -- *  \code
  -- *  SDL_version compiled;
  -- *  SDL_version linked;
  -- *
  -- *  SDL_VERSION(&compiled);
  -- *  SDL_GetVersion(&linked);
  -- *  printf("We compiled against SDL version %d.%d.%d ...\n",
  -- *         compiled.major, compiled.minor, compiled.patch);
  -- *  printf("But we linked against SDL version %d.%d.%d.\n",
  -- *         linked.major, linked.minor, linked.patch);
  -- *  \endcode
  -- *
  -- *  This function may be called safely at any time, even before SDL_Init().
  -- *
  -- *  \sa SDL_VERSION
  --  

   procedure SDL_GetVersion (ver : access SDL_version);  -- ..\SDL2_tmp\SDL_version.h:133
   pragma Import (C, SDL_GetVersion, "SDL_GetVersion");

  --*
  -- *  \brief Get the code revision of SDL that is linked against your program.
  -- *
  -- *  Returns an arbitrary string (a hash value) uniquely identifying the
  -- *  exact revision of the SDL library in use, and is only useful in comparing
  -- *  against other revisions. It is NOT an incrementing number.
  --  

   function SDL_GetRevision return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_version.h:142
   pragma Import (C, SDL_GetRevision, "SDL_GetRevision");

  --*
  -- *  \brief Get the revision number of SDL that is linked against your program.
  -- *
  -- *  Returns a number uniquely identifying the exact revision of the SDL
  -- *  library in use. It is an incrementing number based on commits to
  -- *  hg.libsdl.org.
  --  

   function SDL_GetRevisionNumber return int;  -- ..\SDL2_tmp\SDL_version.h:151
   pragma Import (C, SDL_GetRevisionNumber, "SDL_GetRevisionNumber");

  -- Ends C function definitions when using C++  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_version_h;
