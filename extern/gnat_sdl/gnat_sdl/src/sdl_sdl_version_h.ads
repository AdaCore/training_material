pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_SDL_stdinc_h;
with System;

package SDL_SDL_version_h is


   SDL_MAJOR_VERSION : constant := 1;  --  ../include/SDL/SDL_version.h:42
   SDL_MINOR_VERSION : constant := 2;  --  ../include/SDL/SDL_version.h:43
   SDL_PATCHLEVEL : constant := 15;  --  ../include/SDL/SDL_version.h:44
   --  arg-macro: procedure SDL_VERSION (X)
   --    { (X).major := SDL_MAJOR_VERSION; (X).minor := SDL_MINOR_VERSION; (X).patch := SDL_PATCHLEVEL; }
   --  arg-macro: function SDL_VERSIONNUM (X, Y, Z)
   --    return (X)*1000 + (Y)*100 + (Z);
   --  unsupported macro: SDL_COMPILEDVERSION SDL_VERSIONNUM(SDL_MAJOR_VERSION, SDL_MINOR_VERSION, SDL_PATCHLEVEL)
   --  arg-macro: function SDL_VERSION_ATLEAST (X, Y, Z)
   --    return SDL_COMPILEDVERSION >= SDL_VERSIONNUM(X, Y, Z);

   type SDL_version is record
      major : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_version.h:48
      minor : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_version.h:49
      patch : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_version.h:50
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_version);  -- ../include/SDL/SDL_version.h:47

   function SDL_Linked_Version return System.Address;  -- ../include/SDL/SDL_version.h:83
   pragma Import (C, SDL_Linked_Version, "SDL_Linked_Version");

end SDL_SDL_version_h;
