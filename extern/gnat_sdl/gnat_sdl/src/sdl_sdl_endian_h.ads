pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_SDL_stdinc_h;

package SDL_SDL_endian_h is


   SDL_LIL_ENDIAN : constant := 1234;  --  ../include/SDL/SDL_endian.h:37
   SDL_BIG_ENDIAN : constant := 4321;  --  ../include/SDL/SDL_endian.h:38
   --  unsupported macro: SDL_BYTEORDER SDL_LIL_ENDIAN
   --  arg-macro: function SDL_SwapLE16 (X)
   --    return X;
   --  arg-macro: function SDL_SwapLE32 (X)
   --    return X;
   --  arg-macro: function SDL_SwapLE64 (X)
   --    return X;
   --  arg-macro: procedure SDL_SwapBE16 (X)
   --    SDL_Swap16(X)
   --  arg-macro: procedure SDL_SwapBE32 (X)
   --    SDL_Swap32(X)
   --  arg-macro: procedure SDL_SwapBE64 (X)
   --    SDL_Swap64(X)

   function SDL_Swap16 (x : SDL_SDL_stdinc_h.Uint16) return SDL_SDL_stdinc_h.Uint16;  -- ../include/SDL/SDL_endian.h:75
   pragma Import (C, SDL_Swap16, "SDL_Swap16");

   function SDL_Swap32 (x : SDL_SDL_stdinc_h.Uint32) return SDL_SDL_stdinc_h.Uint32;  -- ../include/SDL/SDL_endian.h:108
   pragma Import (C, SDL_Swap32, "SDL_Swap32");

   function SDL_Swap64 (x : SDL_SDL_stdinc_h.Uint64) return SDL_SDL_stdinc_h.Uint64;  -- ../include/SDL/SDL_endian.h:144
   pragma Import (C, SDL_Swap64, "SDL_Swap64");

end SDL_SDL_endian_h;
