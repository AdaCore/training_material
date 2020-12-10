pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
limited with SDL_rwops_h;
with Interfaces.C.Strings;
with SDL_joystick_h;
with SDL_stdinc_h;

package SDL_gamecontroller_h is

   --  arg-macro: procedure SDL_GameControllerAddMappingsFromFile (file)
   --    SDL_GameControllerAddMappingsFromRW(SDL_RWFromFile(file, "rb"), 1)
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
  -- *  \file SDL_gamecontroller.h
  -- *
  -- *  Include file for SDL game controller event handling
  --  

  -- Set up for C function definitions, even when using C++  
  --*
  -- *  \file SDL_gamecontroller.h
  -- *
  -- *  In order to use these functions, SDL_Init() must have been called
  -- *  with the ::SDL_INIT_GAMECONTROLLER flag.  This causes SDL to scan the system
  -- *  for game controllers, and load appropriate drivers.
  -- *
  -- *  If you would like to receive controller updates while the application
  -- *  is in the background, you should set the following hint before calling
  -- *  SDL_Init(): SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS
  --  

  --*
  -- * The gamecontroller structure used to identify an SDL game controller
  --  

   type u_SDL_GameController is null record;   -- incomplete struct

   subtype SDL_GameController is u_SDL_GameController;  -- ..\SDL2_tmp\SDL_gamecontroller.h:58

   type SDL_GameControllerBindType is 
     (SDL_CONTROLLER_BINDTYPE_NONE,
      SDL_CONTROLLER_BINDTYPE_BUTTON,
      SDL_CONTROLLER_BINDTYPE_AXIS,
      SDL_CONTROLLER_BINDTYPE_HAT);
   pragma Convention (C, SDL_GameControllerBindType);  -- ..\SDL2_tmp\SDL_gamecontroller.h:67

  --*
  -- *  Get the SDL joystick layer binding for this controller button/axis mapping
  --  

   type anon_58 is record
      hat : aliased int;  -- ..\SDL2_tmp\SDL_gamecontroller.h:80
      hat_mask : aliased int;  -- ..\SDL2_tmp\SDL_gamecontroller.h:81
   end record;
   pragma Convention (C_Pass_By_Copy, anon_58);
   type anon_57 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            button : aliased int;  -- ..\SDL2_tmp\SDL_gamecontroller.h:77
         when 1 =>
            axis : aliased int;  -- ..\SDL2_tmp\SDL_gamecontroller.h:78
         when others =>
            hat : aliased anon_58;  -- ..\SDL2_tmp\SDL_gamecontroller.h:82
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, anon_57);
   pragma Unchecked_Union (anon_57);type SDL_GameControllerButtonBind is record
      bindType : aliased SDL_GameControllerBindType;  -- ..\SDL2_tmp\SDL_gamecontroller.h:74
      value : aliased anon_57;  -- ..\SDL2_tmp\SDL_gamecontroller.h:83
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_GameControllerButtonBind);  -- ..\SDL2_tmp\SDL_gamecontroller.h:72

  --*
  -- *  To count the number of game controllers in the system for the following:
  -- *  int nJoysticks = SDL_NumJoysticks();
  -- *  int nGameControllers = 0;
  -- *  for (int i = 0; i < nJoysticks; i++) {
  -- *      if (SDL_IsGameController(i)) {
  -- *          nGameControllers++;
  -- *      }
  -- *  }
  -- *
  -- *  Using the SDL_HINT_GAMECONTROLLERCONFIG hint or the SDL_GameControllerAddMapping() you can add support for controllers SDL is unaware of or cause an existing controller to have a different binding. The format is:
  -- *  guid,name,mappings
  -- *
  -- *  Where GUID is the string value from SDL_JoystickGetGUIDString(), name is the human readable string for the device and mappings are controller mappings to joystick ones.
  -- *  Under Windows there is a reserved GUID of "xinput" that covers any XInput devices.
  -- *  The mapping format for joystick is:
  -- *      bX - a joystick button, index X
  -- *      hX.Y - hat X with value Y
  -- *      aX - axis X of the joystick
  -- *  Buttons can be used as a controller axis and vice versa.
  -- *
  -- *  This string shows an example of a valid mapping for a controller
  -- *  "03000000341a00003608000000000000,PS3 Controller,a:b1,b:b2,y:b3,x:b0,start:b9,guide:b12,back:b8,dpup:h0.1,dpleft:h0.8,dpdown:h0.4,dpright:h0.2,leftshoulder:b4,rightshoulder:b5,leftstick:b10,rightstick:b11,leftx:a0,lefty:a1,rightx:a2,righty:a3,lefttrigger:b6,righttrigger:b7",
  -- *
  --  

  --*
  -- *  Load a set of mappings from a seekable SDL data stream (memory or file), filtered by the current SDL_GetPlatform()
  -- *  A community sourced database of controllers is available at https://raw.github.com/gabomdq/SDL_GameControllerDB/master/gamecontrollerdb.txt
  -- *
  -- *  If \c freerw is non-zero, the stream will be closed after being read.
  -- * 
  -- * \return number of mappings added, -1 on error
  --  

   function SDL_GameControllerAddMappingsFromRW (rw : access SDL_rwops_h.SDL_RWops; freerw : int) return int;  -- ..\SDL2_tmp\SDL_gamecontroller.h:122
   pragma Import (C, SDL_GameControllerAddMappingsFromRW, "SDL_GameControllerAddMappingsFromRW");

  --*
  -- *  Load a set of mappings from a file, filtered by the current SDL_GetPlatform()
  -- *
  -- *  Convenience macro.
  --  

  --*
  -- *  Add or update an existing mapping configuration
  -- *
  -- * \return 1 if mapping is added, 0 if updated, -1 on error
  --  

   function SDL_GameControllerAddMapping (mappingString : Interfaces.C.Strings.chars_ptr) return int;  -- ..\SDL2_tmp\SDL_gamecontroller.h:136
   pragma Import (C, SDL_GameControllerAddMapping, "SDL_GameControllerAddMapping");

  --*
  -- *  Get the number of mappings installed
  -- *
  -- *  \return the number of mappings
  --  

   function SDL_GameControllerNumMappings return int;  -- ..\SDL2_tmp\SDL_gamecontroller.h:143
   pragma Import (C, SDL_GameControllerNumMappings, "SDL_GameControllerNumMappings");

  --*
  -- *  Get the mapping at a particular index.
  -- *
  -- *  \return the mapping string.  Must be freed with SDL_free().  Returns NULL if the index is out of range.
  --  

   function SDL_GameControllerMappingForIndex (mapping_index : int) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_gamecontroller.h:150
   pragma Import (C, SDL_GameControllerMappingForIndex, "SDL_GameControllerMappingForIndex");

  --*
  -- *  Get a mapping string for a GUID
  -- *
  -- *  \return the mapping string.  Must be freed with SDL_free().  Returns NULL if no mapping is available
  --  

   function SDL_GameControllerMappingForGUID (guid : SDL_joystick_h.SDL_JoystickGUID) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_gamecontroller.h:157
   pragma Import (C, SDL_GameControllerMappingForGUID, "SDL_GameControllerMappingForGUID");

  --*
  -- *  Get a mapping string for an open GameController
  -- *
  -- *  \return the mapping string.  Must be freed with SDL_free().  Returns NULL if no mapping is available
  --  

   function SDL_GameControllerMapping (gamecontroller : access SDL_GameController) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_gamecontroller.h:164
   pragma Import (C, SDL_GameControllerMapping, "SDL_GameControllerMapping");

  --*
  -- *  Is the joystick on this index supported by the game controller interface?
  --  

   function SDL_IsGameController (joystick_index : int) return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_gamecontroller.h:169
   pragma Import (C, SDL_IsGameController, "SDL_IsGameController");

  --*
  -- *  Get the implementation dependent name of a game controller.
  -- *  This can be called before any controllers are opened.
  -- *  If no name can be found, this function returns NULL.
  --  

   function SDL_GameControllerNameForIndex (joystick_index : int) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_gamecontroller.h:176
   pragma Import (C, SDL_GameControllerNameForIndex, "SDL_GameControllerNameForIndex");

  --*
  -- *  Get the mapping of a game controller.
  -- *  This can be called before any controllers are opened.
  -- *
  -- *  \return the mapping string.  Must be freed with SDL_free().  Returns NULL if no mapping is available
  --  

   function SDL_GameControllerMappingForDeviceIndex (joystick_index : int) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_gamecontroller.h:184
   pragma Import (C, SDL_GameControllerMappingForDeviceIndex, "SDL_GameControllerMappingForDeviceIndex");

  --*
  -- *  Open a game controller for use.
  -- *  The index passed as an argument refers to the N'th game controller on the system.
  -- *  This index is not the value which will identify this controller in future
  -- *  controller events.  The joystick's instance id (::SDL_JoystickID) will be
  -- *  used there instead.
  -- *
  -- *  \return A controller identifier, or NULL if an error occurred.
  --  

   function SDL_GameControllerOpen (joystick_index : int) return access SDL_GameController;  -- ..\SDL2_tmp\SDL_gamecontroller.h:195
   pragma Import (C, SDL_GameControllerOpen, "SDL_GameControllerOpen");

  --*
  -- * Return the SDL_GameController associated with an instance id.
  --  

   function SDL_GameControllerFromInstanceID (joyid : SDL_joystick_h.SDL_JoystickID) return access SDL_GameController;  -- ..\SDL2_tmp\SDL_gamecontroller.h:200
   pragma Import (C, SDL_GameControllerFromInstanceID, "SDL_GameControllerFromInstanceID");

  --*
  -- *  Return the name for this currently opened controller
  --  

   function SDL_GameControllerName (gamecontroller : access SDL_GameController) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_gamecontroller.h:205
   pragma Import (C, SDL_GameControllerName, "SDL_GameControllerName");

  --*
  -- *  Get the player index of an opened game controller, or -1 if it's not available
  -- *
  -- *  For XInput controllers this returns the XInput user index.
  --  

   function SDL_GameControllerGetPlayerIndex (gamecontroller : access SDL_GameController) return int;  -- ..\SDL2_tmp\SDL_gamecontroller.h:212
   pragma Import (C, SDL_GameControllerGetPlayerIndex, "SDL_GameControllerGetPlayerIndex");

  --*
  -- *  Get the USB vendor ID of an opened controller, if available.
  -- *  If the vendor ID isn't available this function returns 0.
  --  

   function SDL_GameControllerGetVendor (gamecontroller : access SDL_GameController) return SDL_stdinc_h.Uint16;  -- ..\SDL2_tmp\SDL_gamecontroller.h:218
   pragma Import (C, SDL_GameControllerGetVendor, "SDL_GameControllerGetVendor");

  --*
  -- *  Get the USB product ID of an opened controller, if available.
  -- *  If the product ID isn't available this function returns 0.
  --  

   function SDL_GameControllerGetProduct (gamecontroller : access SDL_GameController) return SDL_stdinc_h.Uint16;  -- ..\SDL2_tmp\SDL_gamecontroller.h:224
   pragma Import (C, SDL_GameControllerGetProduct, "SDL_GameControllerGetProduct");

  --*
  -- *  Get the product version of an opened controller, if available.
  -- *  If the product version isn't available this function returns 0.
  --  

   function SDL_GameControllerGetProductVersion (gamecontroller : access SDL_GameController) return SDL_stdinc_h.Uint16;  -- ..\SDL2_tmp\SDL_gamecontroller.h:230
   pragma Import (C, SDL_GameControllerGetProductVersion, "SDL_GameControllerGetProductVersion");

  --*
  -- *  Returns SDL_TRUE if the controller has been opened and currently connected,
  -- *  or SDL_FALSE if it has not.
  --  

   function SDL_GameControllerGetAttached (gamecontroller : access SDL_GameController) return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_gamecontroller.h:236
   pragma Import (C, SDL_GameControllerGetAttached, "SDL_GameControllerGetAttached");

  --*
  -- *  Get the underlying joystick object used by a controller
  --  

   function SDL_GameControllerGetJoystick (gamecontroller : access SDL_GameController) return access SDL_joystick_h.Class_SDL_Joystick.SDL_Joystick;  -- ..\SDL2_tmp\SDL_gamecontroller.h:241
   pragma Import (C, SDL_GameControllerGetJoystick, "SDL_GameControllerGetJoystick");

  --*
  -- *  Enable/disable controller event polling.
  -- *
  -- *  If controller events are disabled, you must call SDL_GameControllerUpdate()
  -- *  yourself and check the state of the controller when you want controller
  -- *  information.
  -- *
  -- *  The state can be one of ::SDL_QUERY, ::SDL_ENABLE or ::SDL_IGNORE.
  --  

   function SDL_GameControllerEventState (state : int) return int;  -- ..\SDL2_tmp\SDL_gamecontroller.h:252
   pragma Import (C, SDL_GameControllerEventState, "SDL_GameControllerEventState");

  --*
  -- *  Update the current state of the open game controllers.
  -- *
  -- *  This is called automatically by the event loop if any game controller
  -- *  events are enabled.
  --  

   procedure SDL_GameControllerUpdate;  -- ..\SDL2_tmp\SDL_gamecontroller.h:260
   pragma Import (C, SDL_GameControllerUpdate, "SDL_GameControllerUpdate");

  --*
  -- *  The list of axes available from a controller
  -- *
  -- *  Thumbstick axis values range from SDL_JOYSTICK_AXIS_MIN to SDL_JOYSTICK_AXIS_MAX,
  -- *  and are centered within ~8000 of zero, though advanced UI will allow users to set
  -- *  or autodetect the dead zone, which varies between controllers.
  -- *
  -- *  Trigger axis values range from 0 to SDL_JOYSTICK_AXIS_MAX.
  --  

   subtype SDL_GameControllerAxis is int;
   SDL_CONTROLLER_AXIS_INVALID : constant int := -1;
   SDL_CONTROLLER_AXIS_LEFTX : constant int := 0;
   SDL_CONTROLLER_AXIS_LEFTY : constant int := 1;
   SDL_CONTROLLER_AXIS_RIGHTX : constant int := 2;
   SDL_CONTROLLER_AXIS_RIGHTY : constant int := 3;
   SDL_CONTROLLER_AXIS_TRIGGERLEFT : constant int := 4;
   SDL_CONTROLLER_AXIS_TRIGGERRIGHT : constant int := 5;
   SDL_CONTROLLER_AXIS_MAX : constant int := 6;  -- ..\SDL2_tmp\SDL_gamecontroller.h:282

  --*
  -- *  turn this string into a axis mapping
  --  

   function SDL_GameControllerGetAxisFromString (pchString : Interfaces.C.Strings.chars_ptr) return SDL_GameControllerAxis;  -- ..\SDL2_tmp\SDL_gamecontroller.h:287
   pragma Import (C, SDL_GameControllerGetAxisFromString, "SDL_GameControllerGetAxisFromString");

  --*
  -- *  turn this axis enum into a string mapping
  --  

   function SDL_GameControllerGetStringForAxis (axis : SDL_GameControllerAxis) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_gamecontroller.h:292
   pragma Import (C, SDL_GameControllerGetStringForAxis, "SDL_GameControllerGetStringForAxis");

  --*
  -- *  Get the SDL joystick layer binding for this controller button mapping
  --  

   function SDL_GameControllerGetBindForAxis (gamecontroller : access SDL_GameController; axis : SDL_GameControllerAxis) return SDL_GameControllerButtonBind;  -- ..\SDL2_tmp\SDL_gamecontroller.h:298
   pragma Import (C, SDL_GameControllerGetBindForAxis, "SDL_GameControllerGetBindForAxis");

  --*
  -- *  Get the current state of an axis control on a game controller.
  -- *
  -- *  The state is a value ranging from -32768 to 32767 (except for the triggers,
  -- *  which range from 0 to 32767).
  -- *
  -- *  The axis indices start at index 0.
  --  

   function SDL_GameControllerGetAxis (gamecontroller : access SDL_GameController; axis : SDL_GameControllerAxis) return SDL_stdinc_h.Sint16;  -- ..\SDL2_tmp\SDL_gamecontroller.h:310
   pragma Import (C, SDL_GameControllerGetAxis, "SDL_GameControllerGetAxis");

  --*
  -- *  The list of buttons available from a controller
  --  

   subtype SDL_GameControllerButton is int;
   SDL_CONTROLLER_BUTTON_INVALID : constant int := -1;
   SDL_CONTROLLER_BUTTON_A : constant int := 0;
   SDL_CONTROLLER_BUTTON_B : constant int := 1;
   SDL_CONTROLLER_BUTTON_X : constant int := 2;
   SDL_CONTROLLER_BUTTON_Y : constant int := 3;
   SDL_CONTROLLER_BUTTON_BACK : constant int := 4;
   SDL_CONTROLLER_BUTTON_GUIDE : constant int := 5;
   SDL_CONTROLLER_BUTTON_START : constant int := 6;
   SDL_CONTROLLER_BUTTON_LEFTSTICK : constant int := 7;
   SDL_CONTROLLER_BUTTON_RIGHTSTICK : constant int := 8;
   SDL_CONTROLLER_BUTTON_LEFTSHOULDER : constant int := 9;
   SDL_CONTROLLER_BUTTON_RIGHTSHOULDER : constant int := 10;
   SDL_CONTROLLER_BUTTON_DPAD_UP : constant int := 11;
   SDL_CONTROLLER_BUTTON_DPAD_DOWN : constant int := 12;
   SDL_CONTROLLER_BUTTON_DPAD_LEFT : constant int := 13;
   SDL_CONTROLLER_BUTTON_DPAD_RIGHT : constant int := 14;
   SDL_CONTROLLER_BUTTON_MAX : constant int := 15;  -- ..\SDL2_tmp\SDL_gamecontroller.h:335

  --*
  -- *  turn this string into a button mapping
  --  

   function SDL_GameControllerGetButtonFromString (pchString : Interfaces.C.Strings.chars_ptr) return SDL_GameControllerButton;  -- ..\SDL2_tmp\SDL_gamecontroller.h:340
   pragma Import (C, SDL_GameControllerGetButtonFromString, "SDL_GameControllerGetButtonFromString");

  --*
  -- *  turn this button enum into a string mapping
  --  

   function SDL_GameControllerGetStringForButton (button : SDL_GameControllerButton) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_gamecontroller.h:345
   pragma Import (C, SDL_GameControllerGetStringForButton, "SDL_GameControllerGetStringForButton");

  --*
  -- *  Get the SDL joystick layer binding for this controller button mapping
  --  

   function SDL_GameControllerGetBindForButton (gamecontroller : access SDL_GameController; button : SDL_GameControllerButton) return SDL_GameControllerButtonBind;  -- ..\SDL2_tmp\SDL_gamecontroller.h:351
   pragma Import (C, SDL_GameControllerGetBindForButton, "SDL_GameControllerGetBindForButton");

  --*
  -- *  Get the current state of a button on a game controller.
  -- *
  -- *  The button indices start at index 0.
  --  

   function SDL_GameControllerGetButton (gamecontroller : access SDL_GameController; button : SDL_GameControllerButton) return SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_gamecontroller.h:360
   pragma Import (C, SDL_GameControllerGetButton, "SDL_GameControllerGetButton");

  --*
  -- *  Trigger a rumble effect
  -- *  Each call to this function cancels any previous rumble effect, and calling it with 0 intensity stops any rumbling.
  -- *
  -- *  \param gamecontroller The controller to vibrate
  -- *  \param low_frequency_rumble The intensity of the low frequency (left) rumble motor, from 0 to 0xFFFF
  -- *  \param high_frequency_rumble The intensity of the high frequency (right) rumble motor, from 0 to 0xFFFF
  -- *  \param duration_ms The duration of the rumble effect, in milliseconds
  -- *
  -- *  \return 0, or -1 if rumble isn't supported on this joystick
  --  

   function SDL_GameControllerRumble
     (gamecontroller : access SDL_GameController;
      low_frequency_rumble : SDL_stdinc_h.Uint16;
      high_frequency_rumble : SDL_stdinc_h.Uint16;
      duration_ms : SDL_stdinc_h.Uint32) return int;  -- ..\SDL2_tmp\SDL_gamecontroller.h:374
   pragma Import (C, SDL_GameControllerRumble, "SDL_GameControllerRumble");

  --*
  -- *  Close a controller previously opened with SDL_GameControllerOpen().
  --  

   procedure SDL_GameControllerClose (gamecontroller : access SDL_GameController);  -- ..\SDL2_tmp\SDL_gamecontroller.h:379
   pragma Import (C, SDL_GameControllerClose, "SDL_GameControllerClose");

  -- Ends C function definitions when using C++  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_gamecontroller_h;
