pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_SDL_stdinc_h;
with SDL_SDL_keysym_h;
with Interfaces.C.Strings;

package SDL_SDL_keyboard_h is


   SDL_ALL_HOTKEYS : constant := 16#FFFFFFFF#;  --  ../include/SDL/SDL_keyboard.h:67

   SDL_DEFAULT_REPEAT_DELAY : constant := 500;  --  ../include/SDL/SDL_keyboard.h:84
   SDL_DEFAULT_REPEAT_INTERVAL : constant := 30;  --  ../include/SDL/SDL_keyboard.h:85

   type SDL_keysym is record
      scancode : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_keyboard.h:60
      sym : aliased SDL_SDL_keysym_h.SDLKey;  -- ../include/SDL/SDL_keyboard.h:61
      c_mod : aliased SDL_SDL_keysym_h.SDLMod;  -- ../include/SDL/SDL_keyboard.h:62
      unicode : aliased SDL_SDL_stdinc_h.Uint16;  -- ../include/SDL/SDL_keyboard.h:63
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_keysym);  -- ../include/SDL/SDL_keyboard.h:59

   function SDL_EnableUNICODE (enable : int) return int;  -- ../include/SDL/SDL_keyboard.h:82
   pragma Import (C, SDL_EnableUNICODE, "SDL_EnableUNICODE");

   function SDL_EnableKeyRepeat (c_delay : int; interval : int) return int;  -- ../include/SDL/SDL_keyboard.h:98
   pragma Import (C, SDL_EnableKeyRepeat, "SDL_EnableKeyRepeat");

   procedure SDL_GetKeyRepeat (c_delay : access int; interval : access int);  -- ../include/SDL/SDL_keyboard.h:99
   pragma Import (C, SDL_GetKeyRepeat, "SDL_GetKeyRepeat");

   function SDL_GetKeyState (numkeys : access int) return access SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_keyboard.h:110
   pragma Import (C, SDL_GetKeyState, "SDL_GetKeyState");

   function SDL_GetModState return SDL_SDL_keysym_h.SDLMod;  -- ../include/SDL/SDL_keyboard.h:115
   pragma Import (C, SDL_GetModState, "SDL_GetModState");

   procedure SDL_SetModState (modstate : SDL_SDL_keysym_h.SDLMod);  -- ../include/SDL/SDL_keyboard.h:121
   pragma Import (C, SDL_SetModState, "SDL_SetModState");

   function SDL_GetKeyName (key : SDL_SDL_keysym_h.SDLKey) return Interfaces.C.Strings.chars_ptr;  -- ../include/SDL/SDL_keyboard.h:126
   pragma Import (C, SDL_GetKeyName, "SDL_GetKeyName");

end SDL_SDL_keyboard_h;
