pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;
with SDL_SDL_stdinc_h;

package SDL_SDL_joystick_h is


   SDL_HAT_CENTERED : constant := 16#00#;  --  ../include/SDL/SDL_joystick.h:141
   SDL_HAT_UP : constant := 16#01#;  --  ../include/SDL/SDL_joystick.h:142
   SDL_HAT_RIGHT : constant := 16#02#;  --  ../include/SDL/SDL_joystick.h:143
   SDL_HAT_DOWN : constant := 16#04#;  --  ../include/SDL/SDL_joystick.h:144
   SDL_HAT_LEFT : constant := 16#08#;  --  ../include/SDL/SDL_joystick.h:145
   --  unsupported macro: SDL_HAT_RIGHTUP (SDL_HAT_RIGHT|SDL_HAT_UP)
   --  unsupported macro: SDL_HAT_RIGHTDOWN (SDL_HAT_RIGHT|SDL_HAT_DOWN)
   --  unsupported macro: SDL_HAT_LEFTUP (SDL_HAT_LEFT|SDL_HAT_UP)
   --  unsupported macro: SDL_HAT_LEFTDOWN (SDL_HAT_LEFT|SDL_HAT_DOWN)

   --  skipped empty struct u_SDL_Joystick

   --  skipped empty struct SDL_Joystick

   function SDL_NumJoysticks return int;  -- ../include/SDL/SDL_joystick.h:53
   pragma Import (C, SDL_NumJoysticks, "SDL_NumJoysticks");

   function SDL_JoystickName (device_index : int) return Interfaces.C.Strings.chars_ptr;  -- ../include/SDL/SDL_joystick.h:61
   pragma Import (C, SDL_JoystickName, "SDL_JoystickName");

   function SDL_JoystickOpen (device_index : int) return System.Address;  -- ../include/SDL/SDL_joystick.h:73
   pragma Import (C, SDL_JoystickOpen, "SDL_JoystickOpen");

   function SDL_JoystickOpened (device_index : int) return int;  -- ../include/SDL/SDL_joystick.h:78
   pragma Import (C, SDL_JoystickOpened, "SDL_JoystickOpened");

   function SDL_JoystickIndex (joystick : System.Address) return int;  -- ../include/SDL/SDL_joystick.h:83
   pragma Import (C, SDL_JoystickIndex, "SDL_JoystickIndex");

   function SDL_JoystickNumAxes (joystick : System.Address) return int;  -- ../include/SDL/SDL_joystick.h:88
   pragma Import (C, SDL_JoystickNumAxes, "SDL_JoystickNumAxes");

   function SDL_JoystickNumBalls (joystick : System.Address) return int;  -- ../include/SDL/SDL_joystick.h:96
   pragma Import (C, SDL_JoystickNumBalls, "SDL_JoystickNumBalls");

   function SDL_JoystickNumHats (joystick : System.Address) return int;  -- ../include/SDL/SDL_joystick.h:101
   pragma Import (C, SDL_JoystickNumHats, "SDL_JoystickNumHats");

   function SDL_JoystickNumButtons (joystick : System.Address) return int;  -- ../include/SDL/SDL_joystick.h:106
   pragma Import (C, SDL_JoystickNumButtons, "SDL_JoystickNumButtons");

   procedure SDL_JoystickUpdate;  -- ../include/SDL/SDL_joystick.h:114
   pragma Import (C, SDL_JoystickUpdate, "SDL_JoystickUpdate");

   function SDL_JoystickEventState (state : int) return int;  -- ../include/SDL/SDL_joystick.h:125
   pragma Import (C, SDL_JoystickEventState, "SDL_JoystickEventState");

   function SDL_JoystickGetAxis (joystick : System.Address; axis : int) return SDL_SDL_stdinc_h.Sint16;  -- ../include/SDL/SDL_joystick.h:134
   pragma Import (C, SDL_JoystickGetAxis, "SDL_JoystickGetAxis");

   function SDL_JoystickGetHat (joystick : System.Address; hat : int) return SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_joystick.h:157
   pragma Import (C, SDL_JoystickGetHat, "SDL_JoystickGetHat");

   function SDL_JoystickGetBall
     (joystick : System.Address;
      ball : int;
      dx : access int;
      dy : access int) return int;  -- ../include/SDL/SDL_joystick.h:166
   pragma Import (C, SDL_JoystickGetBall, "SDL_JoystickGetBall");

   function SDL_JoystickGetButton (joystick : System.Address; button : int) return SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_joystick.h:173
   pragma Import (C, SDL_JoystickGetButton, "SDL_JoystickGetButton");

   procedure SDL_JoystickClose (joystick : System.Address);  -- ../include/SDL/SDL_joystick.h:178
   pragma Import (C, SDL_JoystickClose, "SDL_JoystickClose");

end SDL_SDL_joystick_h;
