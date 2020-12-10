pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

package SDL_SDL_error_h is

   --  arg-macro: procedure SDL_OutOfMemory ()
   --    SDL_Error(SDL_ENOMEM)
   --  arg-macro: procedure SDL_Unsupported ()
   --    SDL_Error(SDL_UNSUPPORTED)
   procedure SDL_SetError (fmt : Interfaces.C.Strings.chars_ptr  -- , ...
      );  -- ../include/SDL/SDL_error.h:43
   pragma Import (C, SDL_SetError, "SDL_SetError");

   function SDL_GetError return Interfaces.C.Strings.chars_ptr;  -- ../include/SDL/SDL_error.h:44
   pragma Import (C, SDL_GetError, "SDL_GetError");

   procedure SDL_ClearError;  -- ../include/SDL/SDL_error.h:45
   pragma Import (C, SDL_ClearError, "SDL_ClearError");

   type SDL_errorcode is 
     (SDL_ENOMEM,
      SDL_EFREAD,
      SDL_EFWRITE,
      SDL_EFSEEK,
      SDL_UNSUPPORTED,
      SDL_LASTERROR);
   pragma Convention (C, SDL_errorcode);  -- ../include/SDL/SDL_error.h:62

   procedure SDL_Error (code : SDL_errorcode);  -- ../include/SDL/SDL_error.h:63
   pragma Import (C, SDL_Error, "SDL_Error");

end SDL_SDL_error_h;
