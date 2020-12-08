pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_stdinc_h;
with SDL_keyboard_h;
with SDL_joystick_h;
with SDL_touch_h;
with SDL_gesture_h;
with Interfaces.C.Strings;
with System;

package SDL_events_h is

   SDL_RELEASED : constant := 0;  --  ..\SDL2_tmp\SDL_events.h:49
   SDL_PRESSED : constant := 1;  --  ..\SDL2_tmp\SDL_events.h:50

   SDL_TEXTEDITINGEVENT_TEXT_SIZE : constant := (32);  --  ..\SDL2_tmp\SDL_events.h:223

   SDL_TEXTINPUTEVENT_TEXT_SIZE : constant := (32);  --  ..\SDL2_tmp\SDL_events.h:238

   SDL_QUERY : constant := -1;  --  ..\SDL2_tmp\SDL_events.h:753
   SDL_IGNORE : constant := 0;  --  ..\SDL2_tmp\SDL_events.h:754
   SDL_DISABLE : constant := 0;  --  ..\SDL2_tmp\SDL_events.h:755
   SDL_ENABLE : constant := 1;  --  ..\SDL2_tmp\SDL_events.h:756
   --  arg-macro: procedure SDL_GetEventState (type)
   --    SDL_EventState(type, SDL_QUERY)

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
  -- *  \file SDL_events.h
  -- *
  -- *  Include file for SDL event handling.
  --  

  -- Set up for C function definitions, even when using C++  
  -- General keyboard/mouse state definitions  
  --*
  -- * \brief The types of events that can be delivered.
  --  

  --*< Unused (do not remove)  
  -- Application events  
  --*< User-requested quit  
  -- These application events have special meaning on iOS, see README-ios.md for details  
  --*< The application is being terminated by the OS
  --                                     Called on iOS in applicationWillTerminate()
  --                                     Called on Android in onDestroy()
  --                                 

  --*< The application is low on memory, free memory if possible.
  --                                     Called on iOS in applicationDidReceiveMemoryWarning()
  --                                     Called on Android in onLowMemory()
  --                                 

  --*< The application is about to enter the background
  --                                     Called on iOS in applicationWillResignActive()
  --                                     Called on Android in onPause()
  --                                 

  --*< The application did enter the background and may not get CPU for some time
  --                                     Called on iOS in applicationDidEnterBackground()
  --                                     Called on Android in onPause()
  --                                 

  --*< The application is about to enter the foreground
  --                                     Called on iOS in applicationWillEnterForeground()
  --                                     Called on Android in onResume()
  --                                 

  --*< The application is now interactive
  --                                     Called on iOS in applicationDidBecomeActive()
  --                                     Called on Android in onResume()
  --                                 

  -- Display events  
  --*< Display state change  
  -- Window events  
  --*< Window state change  
  --*< System specific event  
  -- Keyboard events  
  --*< Key pressed  
  --*< Key released  
  --*< Keyboard text editing (composition)  
  --*< Keyboard text input  
  --*< Keymap changed due to a system event such as an
  --                                     input language or keyboard layout change.
  --                                 

  -- Mouse events  
  --*< Mouse moved  
  --*< Mouse button pressed  
  --*< Mouse button released  
  --*< Mouse wheel motion  
  -- Joystick events  
  --*< Joystick axis motion  
  --*< Joystick trackball motion  
  --*< Joystick hat position change  
  --*< Joystick button pressed  
  --*< Joystick button released  
  --*< A new joystick has been inserted into the system  
  --*< An opened joystick has been removed  
  -- Game controller events  
  --*< Game controller axis motion  
  --*< Game controller button pressed  
  --*< Game controller button released  
  --*< A new Game controller has been inserted into the system  
  --*< An opened Game controller has been removed  
  --*< The controller mapping was updated  
  -- Touch events  
  -- Gesture events  
  -- Clipboard events  
  --*< The clipboard changed  
  -- Drag and drop events  
  --*< The system requests a file open  
  --*< text/plain drag-and-drop event  
  --*< A new set of drops is beginning (NULL filename)  
  --*< Current set of drops is now complete (NULL filename)  
  -- Audio hotplug events  
  --*< A new audio device is available  
  --*< An audio device has been removed.  
  -- Sensor events  
  --*< A sensor was updated  
  -- Render events  
  --*< The render targets have been reset and their contents need to be updated  
  --*< The device has been reset and all textures need to be recreated  
  --* Events ::SDL_USEREVENT through ::SDL_LASTEVENT are for your use,
  --     *  and should be allocated with SDL_RegisterEvents()
  --      

  --*
  --     *  This last event is only for bounding internal arrays
  --      

   subtype SDL_EventType is unsigned;
   SDL_FIRSTEVENT : constant unsigned := 0;
   
   -- manual fix for Ada because of the function with the same name!
   --SDL_QUIT : constant unsigned := 256;
   SDL_QUIT_Evt : constant unsigned := 256;
   
   SDL_APP_TERMINATING : constant unsigned := 257;
   SDL_APP_LOWMEMORY : constant unsigned := 258;
   SDL_APP_WILLENTERBACKGROUND : constant unsigned := 259;
   SDL_APP_DIDENTERBACKGROUND : constant unsigned := 260;
   SDL_APP_WILLENTERFOREGROUND : constant unsigned := 261;
   SDL_APP_DIDENTERFOREGROUND : constant unsigned := 262;

   -- manual fix for Ada because of the function with the same name!
   -- SDL_DISPLAYEVENT : constant unsigned := 336;
   SDL_DISPLAYEVENT_Evt : constant unsigned := 336;

   -- manual fix for Ada because of the function with the same name!
   --   SDL_WINDOWEVENT : constant unsigned := 512;
   SDL_WINDOWEVENT_Evt : constant unsigned := 512;
  
   -- manual fix for Ada because of the function with the same name!
   --  SDL_SYSWMEVENT : constant unsigned := 513;
   SDL_SYSWMEVENT_Evt : constant unsigned := 513;
 
   SDL_KEYDOWN : constant unsigned := 768;
   SDL_KEYUP : constant unsigned := 769;
   SDL_TEXTEDITING : constant unsigned := 770;
   SDL_TEXTINPUT : constant unsigned := 771;
   SDL_KEYMAPCHANGED : constant unsigned := 772;
   SDL_MOUSEMOTION : constant unsigned := 1024;
   SDL_MOUSEBUTTONDOWN : constant unsigned := 1025;
   SDL_MOUSEBUTTONUP : constant unsigned := 1026;
   SDL_MOUSEWHEEL : constant unsigned := 1027;
   SDL_JOYAXISMOTION : constant unsigned := 1536;
   SDL_JOYBALLMOTION : constant unsigned := 1537;
   SDL_JOYHATMOTION : constant unsigned := 1538;
   SDL_JOYBUTTONDOWN : constant unsigned := 1539;
   SDL_JOYBUTTONUP : constant unsigned := 1540;
   SDL_JOYDEVICEADDED : constant unsigned := 1541;
   SDL_JOYDEVICEREMOVED : constant unsigned := 1542;
   SDL_CONTROLLERAXISMOTION : constant unsigned := 1616;
   SDL_CONTROLLERBUTTONDOWN : constant unsigned := 1617;
   SDL_CONTROLLERBUTTONUP : constant unsigned := 1618;
   SDL_CONTROLLERDEVICEADDED : constant unsigned := 1619;
   SDL_CONTROLLERDEVICEREMOVED : constant unsigned := 1620;
   SDL_CONTROLLERDEVICEREMAPPED : constant unsigned := 1621;
   SDL_FINGERDOWN : constant unsigned := 1792;
   SDL_FINGERUP : constant unsigned := 1793;
   SDL_FINGERMOTION : constant unsigned := 1794;
   SDL_DOLLARGESTURE : constant unsigned := 2048;
   SDL_DOLLARRECORD : constant unsigned := 2049;
   SDL_MULTIGESTURE : constant unsigned := 2050;
   SDL_CLIPBOARDUPDATE : constant unsigned := 2304;
   SDL_DROPFILE : constant unsigned := 4096;
   SDL_DROPTEXT : constant unsigned := 4097;
   SDL_DROPBEGIN : constant unsigned := 4098;
   SDL_DROPCOMPLETE : constant unsigned := 4099;
   SDL_AUDIODEVICEADDED : constant unsigned := 4352;
   SDL_AUDIODEVICEREMOVED : constant unsigned := 4353;
   SDL_SENSORUPDATE : constant unsigned := 4608;
   SDL_RENDER_TARGETS_RESET : constant unsigned := 8192;
   SDL_RENDER_DEVICE_RESET : constant unsigned := 8193;
 
   -- manual fix for Ada because of the function with the same name!
   --  SDL_USEREVENT : constant unsigned := 32768;
   SDL_USEREVENT_Evt : constant unsigned := 32768;

   SDL_LASTEVENT : constant unsigned := 65535;  -- ..\SDL2_tmp\SDL_events.h:166

  --*
  -- *  \brief Fields shared by every event
  --  

   type SDL_CommonEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:173
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:174
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_CommonEvent);  -- ..\SDL2_tmp\SDL_events.h:171

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*
  -- *  \brief Display state change event data (event.display.*)
  --  

  --*< ::SDL_DISPLAYEVENT  
   type SDL_DisplayEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:182
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:183
      display : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:184
      event : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:185
      padding1 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:186
      padding2 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:187
      padding3 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:188
      data1 : aliased SDL_stdinc_h.Sint32;  -- ..\SDL2_tmp\SDL_events.h:189
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_DisplayEvent);  -- ..\SDL2_tmp\SDL_events.h:180

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*< The associated display index  
  --*< ::SDL_DisplayEventID  
  --*< event dependent data  
  --*
  -- *  \brief Window state change event data (event.window.*)
  --  

  --*< ::SDL_WINDOWEVENT  
   type SDL_WindowEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:197
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:198
      windowID : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:199
      event : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:200
      padding1 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:201
      padding2 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:202
      padding3 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:203
      data1 : aliased SDL_stdinc_h.Sint32;  -- ..\SDL2_tmp\SDL_events.h:204
      data2 : aliased SDL_stdinc_h.Sint32;  -- ..\SDL2_tmp\SDL_events.h:205
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_WindowEvent);  -- ..\SDL2_tmp\SDL_events.h:195

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*< The associated window  
  --*< ::SDL_WindowEventID  
  --*< event dependent data  
  --*< event dependent data  
  --*
  -- *  \brief Keyboard button event structure (event.key.*)
  --  

  --*< ::SDL_KEYDOWN or ::SDL_KEYUP  
   type SDL_KeyboardEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:213
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:214
      windowID : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:215
      state : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:216
      repeat : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:217
      padding2 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:218
      padding3 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:219
      keysym : aliased SDL_keyboard_h.SDL_Keysym;  -- ..\SDL2_tmp\SDL_events.h:220
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_KeyboardEvent);  -- ..\SDL2_tmp\SDL_events.h:211

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*< The window with keyboard focus, if any  
  --*< ::SDL_PRESSED or ::SDL_RELEASED  
  --*< Non-zero if this is a key repeat  
  --*< The key that was pressed or released  
  --*
  -- *  \brief Keyboard text editing event structure (event.edit.*)
  --  

  --*< ::SDL_TEXTEDITING  
   subtype SDL_TextEditingEvent_text_array is Interfaces.C.char_array (0 .. 31);
   type SDL_TextEditingEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:229
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:230
      windowID : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:231
      text : aliased SDL_TextEditingEvent_text_array;  -- ..\SDL2_tmp\SDL_events.h:232
      start : aliased SDL_stdinc_h.Sint32;  -- ..\SDL2_tmp\SDL_events.h:233
      length : aliased SDL_stdinc_h.Sint32;  -- ..\SDL2_tmp\SDL_events.h:234
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_TextEditingEvent);  -- ..\SDL2_tmp\SDL_events.h:227

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*< The window with keyboard focus, if any  
  --*< The editing text  
  --*< The start cursor of selected editing text  
  --*< The length of selected editing text  
  --*
  -- *  \brief Keyboard text input event structure (event.text.*)
  --  

  --*< ::SDL_TEXTINPUT  
   subtype SDL_TextInputEvent_text_array is Interfaces.C.char_array (0 .. 31);
   type SDL_TextInputEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:244
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:245
      windowID : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:246
      text : aliased SDL_TextInputEvent_text_array;  -- ..\SDL2_tmp\SDL_events.h:247
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_TextInputEvent);  -- ..\SDL2_tmp\SDL_events.h:242

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*< The window with keyboard focus, if any  
  --*< The input text  
  --*
  -- *  \brief Mouse motion event structure (event.motion.*)
  --  

  --*< ::SDL_MOUSEMOTION  
   type SDL_MouseMotionEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:255
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:256
      windowID : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:257
      which : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:258
      state : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:259
      x : aliased SDL_stdinc_h.Sint32;  -- ..\SDL2_tmp\SDL_events.h:260
      y : aliased SDL_stdinc_h.Sint32;  -- ..\SDL2_tmp\SDL_events.h:261
      xrel : aliased SDL_stdinc_h.Sint32;  -- ..\SDL2_tmp\SDL_events.h:262
      yrel : aliased SDL_stdinc_h.Sint32;  -- ..\SDL2_tmp\SDL_events.h:263
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_MouseMotionEvent);  -- ..\SDL2_tmp\SDL_events.h:253

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*< The window with mouse focus, if any  
  --*< The mouse instance id, or SDL_TOUCH_MOUSEID  
  --*< The current button state  
  --*< X coordinate, relative to window  
  --*< Y coordinate, relative to window  
  --*< The relative motion in the X direction  
  --*< The relative motion in the Y direction  
  --*
  -- *  \brief Mouse button event structure (event.button.*)
  --  

  --*< ::SDL_MOUSEBUTTONDOWN or ::SDL_MOUSEBUTTONUP  
   type SDL_MouseButtonEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:271
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:272
      windowID : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:273
      which : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:274
      button : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:275
      state : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:276
      clicks : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:277
      padding1 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:278
      x : aliased SDL_stdinc_h.Sint32;  -- ..\SDL2_tmp\SDL_events.h:279
      y : aliased SDL_stdinc_h.Sint32;  -- ..\SDL2_tmp\SDL_events.h:280
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_MouseButtonEvent);  -- ..\SDL2_tmp\SDL_events.h:269

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*< The window with mouse focus, if any  
  --*< The mouse instance id, or SDL_TOUCH_MOUSEID  
  --*< The mouse button index  
  --*< ::SDL_PRESSED or ::SDL_RELEASED  
  --*< 1 for single-click, 2 for double-click, etc.  
  --*< X coordinate, relative to window  
  --*< Y coordinate, relative to window  
  --*
  -- *  \brief Mouse wheel event structure (event.wheel.*)
  --  

  --*< ::SDL_MOUSEWHEEL  
   type SDL_MouseWheelEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:288
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:289
      windowID : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:290
      which : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:291
      x : aliased SDL_stdinc_h.Sint32;  -- ..\SDL2_tmp\SDL_events.h:292
      y : aliased SDL_stdinc_h.Sint32;  -- ..\SDL2_tmp\SDL_events.h:293
      direction : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:294
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_MouseWheelEvent);  -- ..\SDL2_tmp\SDL_events.h:286

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*< The window with mouse focus, if any  
  --*< The mouse instance id, or SDL_TOUCH_MOUSEID  
  --*< The amount scrolled horizontally, positive to the right and negative to the left  
  --*< The amount scrolled vertically, positive away from the user and negative toward the user  
  --*< Set to one of the SDL_MOUSEWHEEL_* defines. When FLIPPED the values in X and Y will be opposite. Multiply by -1 to change them back  
  --*
  -- *  \brief Joystick axis motion event structure (event.jaxis.*)
  --  

  --*< ::SDL_JOYAXISMOTION  
   type SDL_JoyAxisEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:302
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:303
      which : aliased SDL_joystick_h.SDL_JoystickID;  -- ..\SDL2_tmp\SDL_events.h:304
      axis : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:305
      padding1 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:306
      padding2 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:307
      padding3 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:308
      value : aliased SDL_stdinc_h.Sint16;  -- ..\SDL2_tmp\SDL_events.h:309
      padding4 : aliased SDL_stdinc_h.Uint16;  -- ..\SDL2_tmp\SDL_events.h:310
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_JoyAxisEvent);  -- ..\SDL2_tmp\SDL_events.h:300

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*< The joystick instance id  
  --*< The joystick axis index  
  --*< The axis value (range: -32768 to 32767)  
  --*
  -- *  \brief Joystick trackball motion event structure (event.jball.*)
  --  

  --*< ::SDL_JOYBALLMOTION  
   type SDL_JoyBallEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:318
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:319
      which : aliased SDL_joystick_h.SDL_JoystickID;  -- ..\SDL2_tmp\SDL_events.h:320
      ball : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:321
      padding1 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:322
      padding2 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:323
      padding3 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:324
      xrel : aliased SDL_stdinc_h.Sint16;  -- ..\SDL2_tmp\SDL_events.h:325
      yrel : aliased SDL_stdinc_h.Sint16;  -- ..\SDL2_tmp\SDL_events.h:326
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_JoyBallEvent);  -- ..\SDL2_tmp\SDL_events.h:316

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*< The joystick instance id  
  --*< The joystick trackball index  
  --*< The relative motion in the X direction  
  --*< The relative motion in the Y direction  
  --*
  -- *  \brief Joystick hat position change event structure (event.jhat.*)
  --  

  --*< ::SDL_JOYHATMOTION  
   type SDL_JoyHatEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:334
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:335
      which : aliased SDL_joystick_h.SDL_JoystickID;  -- ..\SDL2_tmp\SDL_events.h:336
      hat : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:337
      value : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:338
      padding1 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:345
      padding2 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:346
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_JoyHatEvent);  -- ..\SDL2_tmp\SDL_events.h:332

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*< The joystick instance id  
  --*< The joystick hat index  
  --*< The hat position value.
  --                         *   \sa ::SDL_HAT_LEFTUP ::SDL_HAT_UP ::SDL_HAT_RIGHTUP
  --                         *   \sa ::SDL_HAT_LEFT ::SDL_HAT_CENTERED ::SDL_HAT_RIGHT
  --                         *   \sa ::SDL_HAT_LEFTDOWN ::SDL_HAT_DOWN ::SDL_HAT_RIGHTDOWN
  --                         *
  --                         *   Note that zero means the POV is centered.
  --                          

  --*
  -- *  \brief Joystick button event structure (event.jbutton.*)
  --  

  --*< ::SDL_JOYBUTTONDOWN or ::SDL_JOYBUTTONUP  
   type SDL_JoyButtonEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:354
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:355
      which : aliased SDL_joystick_h.SDL_JoystickID;  -- ..\SDL2_tmp\SDL_events.h:356
      button : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:357
      state : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:358
      padding1 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:359
      padding2 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:360
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_JoyButtonEvent);  -- ..\SDL2_tmp\SDL_events.h:352

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*< The joystick instance id  
  --*< The joystick button index  
  --*< ::SDL_PRESSED or ::SDL_RELEASED  
  --*
  -- *  \brief Joystick device event structure (event.jdevice.*)
  --  

  --*< ::SDL_JOYDEVICEADDED or ::SDL_JOYDEVICEREMOVED  
   type SDL_JoyDeviceEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:368
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:369
      which : aliased SDL_stdinc_h.Sint32;  -- ..\SDL2_tmp\SDL_events.h:370
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_JoyDeviceEvent);  -- ..\SDL2_tmp\SDL_events.h:366

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*< The joystick device index for the ADDED event, instance id for the REMOVED event  
  --*
  -- *  \brief Game controller axis motion event structure (event.caxis.*)
  --  

  --*< ::SDL_CONTROLLERAXISMOTION  
   type SDL_ControllerAxisEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:379
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:380
      which : aliased SDL_joystick_h.SDL_JoystickID;  -- ..\SDL2_tmp\SDL_events.h:381
      axis : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:382
      padding1 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:383
      padding2 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:384
      padding3 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:385
      value : aliased SDL_stdinc_h.Sint16;  -- ..\SDL2_tmp\SDL_events.h:386
      padding4 : aliased SDL_stdinc_h.Uint16;  -- ..\SDL2_tmp\SDL_events.h:387
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_ControllerAxisEvent);  -- ..\SDL2_tmp\SDL_events.h:377

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*< The joystick instance id  
  --*< The controller axis (SDL_GameControllerAxis)  
  --*< The axis value (range: -32768 to 32767)  
  --*
  -- *  \brief Game controller button event structure (event.cbutton.*)
  --  

  --*< ::SDL_CONTROLLERBUTTONDOWN or ::SDL_CONTROLLERBUTTONUP  
   type SDL_ControllerButtonEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:396
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:397
      which : aliased SDL_joystick_h.SDL_JoystickID;  -- ..\SDL2_tmp\SDL_events.h:398
      button : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:399
      state : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:400
      padding1 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:401
      padding2 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:402
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_ControllerButtonEvent);  -- ..\SDL2_tmp\SDL_events.h:394

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*< The joystick instance id  
  --*< The controller button (SDL_GameControllerButton)  
  --*< ::SDL_PRESSED or ::SDL_RELEASED  
  --*
  -- *  \brief Controller device event structure (event.cdevice.*)
  --  

  --*< ::SDL_CONTROLLERDEVICEADDED, ::SDL_CONTROLLERDEVICEREMOVED, or ::SDL_CONTROLLERDEVICEREMAPPED  
   type SDL_ControllerDeviceEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:411
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:412
      which : aliased SDL_stdinc_h.Sint32;  -- ..\SDL2_tmp\SDL_events.h:413
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_ControllerDeviceEvent);  -- ..\SDL2_tmp\SDL_events.h:409

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*< The joystick device index for the ADDED event, instance id for the REMOVED or REMAPPED event  
  --*
  -- *  \brief Audio device event structure (event.adevice.*)
  --  

  --*< ::SDL_AUDIODEVICEADDED, or ::SDL_AUDIODEVICEREMOVED  
   type SDL_AudioDeviceEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:421
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:422
      which : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:423
      iscapture : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:424
      padding1 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:425
      padding2 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:426
      padding3 : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:427
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_AudioDeviceEvent);  -- ..\SDL2_tmp\SDL_events.h:419

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*< The audio device index for the ADDED event (valid until next SDL_GetNumAudioDevices() call), SDL_AudioDeviceID for the REMOVED event  
  --*< zero if an output device, non-zero if a capture device.  
  --*
  -- *  \brief Touch finger event structure (event.tfinger.*)
  --  

  --*< ::SDL_FINGERMOTION or ::SDL_FINGERDOWN or ::SDL_FINGERUP  
   type SDL_TouchFingerEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:436
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:437
      touchId : aliased SDL_touch_h.SDL_TouchID;  -- ..\SDL2_tmp\SDL_events.h:438
      fingerId : aliased SDL_touch_h.SDL_FingerID;  -- ..\SDL2_tmp\SDL_events.h:439
      x : aliased float;  -- ..\SDL2_tmp\SDL_events.h:440
      y : aliased float;  -- ..\SDL2_tmp\SDL_events.h:441
      dx : aliased float;  -- ..\SDL2_tmp\SDL_events.h:442
      dy : aliased float;  -- ..\SDL2_tmp\SDL_events.h:443
      pressure : aliased float;  -- ..\SDL2_tmp\SDL_events.h:444
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_TouchFingerEvent);  -- ..\SDL2_tmp\SDL_events.h:434

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*< The touch device id  
  --*< Normalized in the range 0...1  
  --*< Normalized in the range 0...1  
  --*< Normalized in the range -1...1  
  --*< Normalized in the range -1...1  
  --*< Normalized in the range 0...1  
  --*
  -- *  \brief Multiple Finger Gesture Event (event.mgesture.*)
  --  

  --*< ::SDL_MULTIGESTURE  
   type SDL_MultiGestureEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:453
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:454
      touchId : aliased SDL_touch_h.SDL_TouchID;  -- ..\SDL2_tmp\SDL_events.h:455
      dTheta : aliased float;  -- ..\SDL2_tmp\SDL_events.h:456
      dDist : aliased float;  -- ..\SDL2_tmp\SDL_events.h:457
      x : aliased float;  -- ..\SDL2_tmp\SDL_events.h:458
      y : aliased float;  -- ..\SDL2_tmp\SDL_events.h:459
      numFingers : aliased SDL_stdinc_h.Uint16;  -- ..\SDL2_tmp\SDL_events.h:460
      padding : aliased SDL_stdinc_h.Uint16;  -- ..\SDL2_tmp\SDL_events.h:461
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_MultiGestureEvent);  -- ..\SDL2_tmp\SDL_events.h:451

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*< The touch device id  
  --*
  -- * \brief Dollar Gesture Event (event.dgesture.*)
  --  

  --*< ::SDL_DOLLARGESTURE or ::SDL_DOLLARRECORD  
   type SDL_DollarGestureEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:470
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:471
      touchId : aliased SDL_touch_h.SDL_TouchID;  -- ..\SDL2_tmp\SDL_events.h:472
      gestureId : aliased SDL_gesture_h.SDL_GestureID;  -- ..\SDL2_tmp\SDL_events.h:473
      numFingers : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:474
      error : aliased float;  -- ..\SDL2_tmp\SDL_events.h:475
      x : aliased float;  -- ..\SDL2_tmp\SDL_events.h:476
      y : aliased float;  -- ..\SDL2_tmp\SDL_events.h:477
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_DollarGestureEvent);  -- ..\SDL2_tmp\SDL_events.h:468

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*< The touch device id  
  --*< Normalized center of gesture  
  --*< Normalized center of gesture  
  --*
  -- *  \brief An event used to request a file open by the system (event.drop.*)
  -- *         This event is enabled by default, you can disable it with SDL_EventState().
  -- *  \note If this event is enabled, you must free the filename in the event.
  --  

  --*< ::SDL_DROPBEGIN or ::SDL_DROPFILE or ::SDL_DROPTEXT or ::SDL_DROPCOMPLETE  
   type SDL_DropEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:488
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:489
      file : Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_events.h:490
      windowID : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:491
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_DropEvent);  -- ..\SDL2_tmp\SDL_events.h:486

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*< The file name, which should be freed with SDL_free(), is NULL on begin/complete  
  --*< The window that was dropped on, if any  
  --*
  -- *  \brief Sensor event structure (event.sensor.*)
  --  

  --*< ::SDL_SENSORUPDATE  
   type SDL_SensorEvent_data_array is array (0 .. 5) of aliased float;
   type SDL_SensorEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:500
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:501
      which : aliased SDL_stdinc_h.Sint32;  -- ..\SDL2_tmp\SDL_events.h:502
      data : aliased SDL_SensorEvent_data_array;  -- ..\SDL2_tmp\SDL_events.h:503
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_SensorEvent);  -- ..\SDL2_tmp\SDL_events.h:498

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*< The instance ID of the sensor  
  --*< Up to 6 values from the sensor - additional values can be queried using SDL_SensorGetData()  
  --*
  -- *  \brief The "quit requested" event
  --  

  --*< ::SDL_QUIT  
   type SDL_QuitEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:511
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:512
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_QuitEvent);  -- ..\SDL2_tmp\SDL_events.h:509

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*
  -- *  \brief OS Specific event
  --  

  --*< ::SDL_QUIT  
   type SDL_OSEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:520
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:521
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_OSEvent);  -- ..\SDL2_tmp\SDL_events.h:518

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*
  -- *  \brief A user-defined event type (event.user.*)
  --  

  --*< ::SDL_USEREVENT through ::SDL_LASTEVENT-1  
   type SDL_UserEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:529
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:530
      windowID : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:531
      code : aliased SDL_stdinc_h.Sint32;  -- ..\SDL2_tmp\SDL_events.h:532
      data1 : System.Address;  -- ..\SDL2_tmp\SDL_events.h:533
      data2 : System.Address;  -- ..\SDL2_tmp\SDL_events.h:534
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_UserEvent);  -- ..\SDL2_tmp\SDL_events.h:527

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*< The associated window if any  
  --*< User defined event code  
  --*< User defined data pointer  
  --*< User defined data pointer  
   type SDL_SysWMmsg is null record;   -- incomplete struct

  --*
  -- *  \brief A video driver dependent system event (event.syswm.*)
  -- *         This event is disabled by default, you can enable it with SDL_EventState()
  -- *
  -- *  \note If you want to use this event, you should include SDL_syswm.h.
  --  

  --*< ::SDL_SYSWMEVENT  
   type SDL_SysWMEvent is record
      c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:549
      timestamp : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:550
      msg : access SDL_SysWMmsg;  -- ..\SDL2_tmp\SDL_events.h:551
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_SysWMEvent);  -- ..\SDL2_tmp\SDL_events.h:547

  --*< In milliseconds, populated using SDL_GetTicks()  
  --*< driver dependent data, defined in SDL_syswm.h  
  --*
  -- *  \brief General event structure
  --  

  --*< Event type, shared with all events  
   type SDL_Event_padding_array is array (0 .. 55) of aliased SDL_stdinc_h.Uint8;
   type SDL_Event (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            c_type : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:559
         when 1 =>
            common : aliased SDL_CommonEvent;  -- ..\SDL2_tmp\SDL_events.h:560
         when 2 =>
            display : aliased SDL_DisplayEvent;  -- ..\SDL2_tmp\SDL_events.h:561
         when 3 =>
            window : aliased SDL_WindowEvent;  -- ..\SDL2_tmp\SDL_events.h:562
         when 4 =>
            key : aliased SDL_KeyboardEvent;  -- ..\SDL2_tmp\SDL_events.h:563
         when 5 =>
            edit : aliased SDL_TextEditingEvent;  -- ..\SDL2_tmp\SDL_events.h:564
         when 6 =>
            text : aliased SDL_TextInputEvent;  -- ..\SDL2_tmp\SDL_events.h:565
         when 7 =>
            motion : aliased SDL_MouseMotionEvent;  -- ..\SDL2_tmp\SDL_events.h:566
         when 8 =>
            button : aliased SDL_MouseButtonEvent;  -- ..\SDL2_tmp\SDL_events.h:567
         when 9 =>
            wheel : aliased SDL_MouseWheelEvent;  -- ..\SDL2_tmp\SDL_events.h:568
         when 10 =>
            jaxis : aliased SDL_JoyAxisEvent;  -- ..\SDL2_tmp\SDL_events.h:569
         when 11 =>
            jball : aliased SDL_JoyBallEvent;  -- ..\SDL2_tmp\SDL_events.h:570
         when 12 =>
            jhat : aliased SDL_JoyHatEvent;  -- ..\SDL2_tmp\SDL_events.h:571
         when 13 =>
            jbutton : aliased SDL_JoyButtonEvent;  -- ..\SDL2_tmp\SDL_events.h:572
         when 14 =>
            jdevice : aliased SDL_JoyDeviceEvent;  -- ..\SDL2_tmp\SDL_events.h:573
         when 15 =>
            caxis : aliased SDL_ControllerAxisEvent;  -- ..\SDL2_tmp\SDL_events.h:574
         when 16 =>
            cbutton : aliased SDL_ControllerButtonEvent;  -- ..\SDL2_tmp\SDL_events.h:575
         when 17 =>
            cdevice : aliased SDL_ControllerDeviceEvent;  -- ..\SDL2_tmp\SDL_events.h:576
         when 18 =>
            adevice : aliased SDL_AudioDeviceEvent;  -- ..\SDL2_tmp\SDL_events.h:577
         when 19 =>
            sensor : aliased SDL_SensorEvent;  -- ..\SDL2_tmp\SDL_events.h:578
         when 20 =>
            quit : aliased SDL_QuitEvent;  -- ..\SDL2_tmp\SDL_events.h:579
         when 21 =>
            user : aliased SDL_UserEvent;  -- ..\SDL2_tmp\SDL_events.h:580
         when 22 =>
            syswm : aliased SDL_SysWMEvent;  -- ..\SDL2_tmp\SDL_events.h:581
         when 23 =>
            tfinger : aliased SDL_TouchFingerEvent;  -- ..\SDL2_tmp\SDL_events.h:582
         when 24 =>
            mgesture : aliased SDL_MultiGestureEvent;  -- ..\SDL2_tmp\SDL_events.h:583
         when 25 =>
            dgesture : aliased SDL_DollarGestureEvent;  -- ..\SDL2_tmp\SDL_events.h:584
         when 26 =>
            drop : aliased SDL_DropEvent;  -- ..\SDL2_tmp\SDL_events.h:585
         when others =>
            padding : aliased SDL_Event_padding_array;  -- ..\SDL2_tmp\SDL_events.h:594
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_Event);
   pragma Unchecked_Union (SDL_Event);  -- ..\SDL2_tmp\SDL_events.h:557

  --*< Common event data  
  --*< Window event data  
  --*< Window event data  
  --*< Keyboard event data  
  --*< Text editing event data  
  --*< Text input event data  
  --*< Mouse motion event data  
  --*< Mouse button event data  
  --*< Mouse wheel event data  
  --*< Joystick axis event data  
  --*< Joystick ball event data  
  --*< Joystick hat event data  
  --*< Joystick button event data  
  --*< Joystick device change event data  
  --*< Game Controller axis event data  
  --*< Game Controller button event data  
  --*< Game Controller device event data  
  --*< Audio device event data  
  --*< Sensor event data  
  --*< Quit request event data  
  --*< Custom event data  
  --*< System dependent window event data  
  --*< Touch finger event data  
  --*< Gesture event data  
  --*< Gesture event data  
  --*< Drag and drop event data  
  -- This is necessary for ABI compatibility between Visual C++ and GCC
  --       Visual C++ will respect the push pack pragma and use 52 bytes for
  --       this structure, and GCC will use the alignment of the largest datatype
  --       within the union, which is 8 bytes.
  --       So... we'll add padding to force the size to be 56 bytes for both.
  --     

  -- Function prototypes  
  --*
  -- *  Pumps the event loop, gathering events from the input devices.
  -- *
  -- *  This function updates the event queue and internal input device state.
  -- *
  -- *  This should only be run in the thread that sets the video mode.
  --  

   procedure SDL_PumpEvents;  -- ..\SDL2_tmp\SDL_events.h:607
   pragma Import (C, SDL_PumpEvents, "SDL_PumpEvents");

  -- @{  
   type SDL_eventaction is 
     (SDL_ADDEVENT,
      SDL_PEEKEVENT,
      SDL_GETEVENT);
   pragma Convention (C, SDL_eventaction);  -- ..\SDL2_tmp\SDL_events.h:615

  --*
  -- *  Checks the event queue for messages and optionally returns them.
  -- *
  -- *  If \c action is ::SDL_ADDEVENT, up to \c numevents events will be added to
  -- *  the back of the event queue.
  -- *
  -- *  If \c action is ::SDL_PEEKEVENT, up to \c numevents events at the front
  -- *  of the event queue, within the specified minimum and maximum type,
  -- *  will be returned and will not be removed from the queue.
  -- *
  -- *  If \c action is ::SDL_GETEVENT, up to \c numevents events at the front
  -- *  of the event queue, within the specified minimum and maximum type,
  -- *  will be returned and will be removed from the queue.
  -- *
  -- *  \return The number of events actually stored, or -1 if there was an error.
  -- *
  -- *  This function is thread-safe.
  --  

   function SDL_PeepEvents
     (events : access SDL_Event;
      numevents : int;
      action : SDL_eventaction;
      minType : SDL_stdinc_h.Uint32;
      maxType : SDL_stdinc_h.Uint32) return int;  -- ..\SDL2_tmp\SDL_events.h:635
   pragma Import (C, SDL_PeepEvents, "SDL_PeepEvents");

  -- @}  
  --*
  -- *  Checks to see if certain event types are in the event queue.
  --  

   function SDL_HasEvent (c_type : SDL_stdinc_h.Uint32) return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_events.h:643
   pragma Import (C, SDL_HasEvent, "SDL_HasEvent");

   function SDL_HasEvents (minType : SDL_stdinc_h.Uint32; maxType : SDL_stdinc_h.Uint32) return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_events.h:644
   pragma Import (C, SDL_HasEvents, "SDL_HasEvents");

  --*
  -- *  This function clears events from the event queue
  -- *  This function only affects currently queued events. If you want to make
  -- *  sure that all pending OS events are flushed, you can call SDL_PumpEvents()
  -- *  on the main thread immediately before the flush call.
  --  

   procedure SDL_FlushEvent (c_type : SDL_stdinc_h.Uint32);  -- ..\SDL2_tmp\SDL_events.h:652
   pragma Import (C, SDL_FlushEvent, "SDL_FlushEvent");

   procedure SDL_FlushEvents (minType : SDL_stdinc_h.Uint32; maxType : SDL_stdinc_h.Uint32);  -- ..\SDL2_tmp\SDL_events.h:653
   pragma Import (C, SDL_FlushEvents, "SDL_FlushEvents");

  --*
  -- *  \brief Polls for currently pending events.
  -- *
  -- *  \return 1 if there are any pending events, or 0 if there are none available.
  -- *
  -- *  \param event If not NULL, the next event is removed from the queue and
  -- *               stored in that area.
  --  

   function SDL_PollEvent (event : access SDL_Event) return int;  -- ..\SDL2_tmp\SDL_events.h:663
   pragma Import (C, SDL_PollEvent, "SDL_PollEvent");

  --*
  -- *  \brief Waits indefinitely for the next available event.
  -- *
  -- *  \return 1, or 0 if there was an error while waiting for events.
  -- *
  -- *  \param event If not NULL, the next event is removed from the queue and
  -- *               stored in that area.
  --  

   function SDL_WaitEvent (event : access SDL_Event) return int;  -- ..\SDL2_tmp\SDL_events.h:673
   pragma Import (C, SDL_WaitEvent, "SDL_WaitEvent");

  --*
  -- *  \brief Waits until the specified timeout (in milliseconds) for the next
  -- *         available event.
  -- *
  -- *  \return 1, or 0 if there was an error while waiting for events.
  -- *
  -- *  \param event If not NULL, the next event is removed from the queue and
  -- *               stored in that area.
  -- *  \param timeout The timeout (in milliseconds) to wait for next event.
  --  

   function SDL_WaitEventTimeout (event : access SDL_Event; timeout : int) return int;  -- ..\SDL2_tmp\SDL_events.h:685
   pragma Import (C, SDL_WaitEventTimeout, "SDL_WaitEventTimeout");

  --*
  -- *  \brief Add an event to the event queue.
  -- *
  -- *  \return 1 on success, 0 if the event was filtered, or -1 if the event queue
  -- *          was full or there was some other error.
  --  

   function SDL_PushEvent (event : access SDL_Event) return int;  -- ..\SDL2_tmp\SDL_events.h:694
   pragma Import (C, SDL_PushEvent, "SDL_PushEvent");

   type SDL_EventFilter is access function (arg1 : System.Address; arg2 : access SDL_Event) return int;
   pragma Convention (C, SDL_EventFilter);  -- ..\SDL2_tmp\SDL_events.h:696

  --*
  -- *  Sets up a filter to process all events before they change internal state and
  -- *  are posted to the internal event queue.
  -- *
  -- *  The filter is prototyped as:
  -- *  \code
  -- *      int SDL_EventFilter(void *userdata, SDL_Event * event);
  -- *  \endcode
  -- *
  -- *  If the filter returns 1, then the event will be added to the internal queue.
  -- *  If it returns 0, then the event will be dropped from the queue, but the
  -- *  internal state will still be updated.  This allows selective filtering of
  -- *  dynamically arriving events.
  -- *
  -- *  \warning  Be very careful of what you do in the event filter function, as
  -- *            it may run in a different thread!
  -- *
  -- *  There is one caveat when dealing with the ::SDL_QuitEvent event type.  The
  -- *  event filter is only called when the window manager desires to close the
  -- *  application window.  If the event filter returns 1, then the window will
  -- *  be closed, otherwise the window will remain open if possible.
  -- *
  -- *  If the quit event is generated by an interrupt signal, it will bypass the
  -- *  internal queue and be delivered to the application at the next event poll.
  --  

   procedure SDL_SetEventFilter (filter : SDL_EventFilter; userdata : System.Address);  -- ..\SDL2_tmp\SDL_events.h:723
   pragma Import (C, SDL_SetEventFilter, "SDL_SetEventFilter");

  --*
  -- *  Return the current event filter - can be used to "chain" filters.
  -- *  If there is no event filter set, this function returns SDL_FALSE.
  --  

   function SDL_GetEventFilter (filter : System.Address; userdata : System.Address) return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_events.h:730
   pragma Import (C, SDL_GetEventFilter, "SDL_GetEventFilter");

  --*
  -- *  Add a function which is called when an event is added to the queue.
  --  

   procedure SDL_AddEventWatch (filter : SDL_EventFilter; userdata : System.Address);  -- ..\SDL2_tmp\SDL_events.h:736
   pragma Import (C, SDL_AddEventWatch, "SDL_AddEventWatch");

  --*
  -- *  Remove an event watch function added with SDL_AddEventWatch()
  --  

   procedure SDL_DelEventWatch (filter : SDL_EventFilter; userdata : System.Address);  -- ..\SDL2_tmp\SDL_events.h:742
   pragma Import (C, SDL_DelEventWatch, "SDL_DelEventWatch");

  --*
  -- *  Run the filter function on the current event queue, removing any
  -- *  events for which the filter returns 0.
  --  

   procedure SDL_FilterEvents (filter : SDL_EventFilter; userdata : System.Address);  -- ..\SDL2_tmp\SDL_events.h:749
   pragma Import (C, SDL_FilterEvents, "SDL_FilterEvents");

  -- @{  
  --*
  -- *  This function allows you to set the state of processing certain events.
  -- *   - If \c state is set to ::SDL_IGNORE, that event will be automatically
  -- *     dropped from the event queue and will not be filtered.
  -- *   - If \c state is set to ::SDL_ENABLE, that event will be processed
  -- *     normally.
  -- *   - If \c state is set to ::SDL_QUERY, SDL_EventState() will return the
  -- *     current processing state of the specified event.
  --  

   function SDL_EventState (c_type : SDL_stdinc_h.Uint32; state : int) return SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_events.h:767
   pragma Import (C, SDL_EventState, "SDL_EventState");

  -- @}  
  --*
  -- *  This function allocates a set of user-defined events, and returns
  -- *  the beginning event number for that set of events.
  -- *
  -- *  If there aren't enough user-defined events left, this function
  -- *  returns (Uint32)-1
  --  

   function SDL_RegisterEvents (numevents : int) return SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_events.h:778
   pragma Import (C, SDL_RegisterEvents, "SDL_RegisterEvents");

  -- Ends C function definitions when using C++  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_events_h;
