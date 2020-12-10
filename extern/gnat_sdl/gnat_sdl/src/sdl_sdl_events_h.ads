pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_SDL_stdinc_h;
with SDL_SDL_keyboard_h;
with System;

package SDL_SDL_events_h is


   SDL_RELEASED : constant := 0;  --  ../include/SDL/SDL_events.h:47
   SDL_PRESSED : constant := 1;  --  ../include/SDL/SDL_events.h:48
   --  arg-macro: function SDL_EVENTMASK (X)
   --    return 2**(X);

   SDL_ALLEVENTS : constant := 16#FFFFFFFF#;  --  ../include/SDL/SDL_events.h:115

   SDL_QUERY : constant := -1;  --  ../include/SDL/SDL_events.h:334
   SDL_IGNORE : constant := 0;  --  ../include/SDL/SDL_events.h:335
   SDL_DISABLE : constant := 0;  --  ../include/SDL/SDL_events.h:336
   SDL_ENABLE : constant := 1;  --  ../include/SDL/SDL_events.h:337

   subtype SDL_EventType is unsigned;
   SDL_NOEVENT : constant SDL_EventType := 0;
   SDL_ACTIVEEVENT : constant SDL_EventType := 1;
   SDL_KEYDOWN : constant SDL_EventType := 2;
   SDL_KEYUP : constant SDL_EventType := 3;
   SDL_MOUSEMOTION : constant SDL_EventType := 4;
   SDL_MOUSEBUTTONDOWN : constant SDL_EventType := 5;
   SDL_MOUSEBUTTONUP : constant SDL_EventType := 6;
   SDL_JOYAXISMOTION : constant SDL_EventType := 7;
   SDL_JOYBALLMOTION : constant SDL_EventType := 8;
   SDL_JOYHATMOTION : constant SDL_EventType := 9;
   SDL_JOYBUTTONDOWN : constant SDL_EventType := 10;
   SDL_JOYBUTTONUP : constant SDL_EventType := 11;
   SDL_QUIT : constant SDL_EventType := 12;
   SDL_SYSWMEVENT : constant SDL_EventType := 13;
   SDL_EVENT_RESERVEDA : constant SDL_EventType := 14;
   SDL_EVENT_RESERVEDB : constant SDL_EventType := 15;
   SDL_VIDEORESIZE : constant SDL_EventType := 16;
   SDL_VIDEOEXPOSE : constant SDL_EventType := 17;
   SDL_EVENT_RESERVED2 : constant SDL_EventType := 18;
   SDL_EVENT_RESERVED3 : constant SDL_EventType := 19;
   SDL_EVENT_RESERVED4 : constant SDL_EventType := 20;
   SDL_EVENT_RESERVED5 : constant SDL_EventType := 21;
   SDL_EVENT_RESERVED6 : constant SDL_EventType := 22;
   SDL_EVENT_RESERVED7 : constant SDL_EventType := 23;
   SDL_USEREVENT : constant SDL_EventType := 24;
   SDL_NUMEVENTS : constant SDL_EventType := 32;  -- ../include/SDL/SDL_events.h:83

   subtype SDL_EventMask is unsigned;
   SDL_ACTIVEEVENTMASK : constant SDL_EventMask := 2;
   SDL_KEYDOWNMASK : constant SDL_EventMask := 4;
   SDL_KEYUPMASK : constant SDL_EventMask := 8;
   SDL_KEYEVENTMASK : constant SDL_EventMask := 12;
   SDL_MOUSEMOTIONMASK : constant SDL_EventMask := 16;
   SDL_MOUSEBUTTONDOWNMASK : constant SDL_EventMask := 32;
   SDL_MOUSEBUTTONUPMASK : constant SDL_EventMask := 64;
   SDL_MOUSEEVENTMASK : constant SDL_EventMask := 112;
   SDL_JOYAXISMOTIONMASK : constant SDL_EventMask := 128;
   SDL_JOYBALLMOTIONMASK : constant SDL_EventMask := 256;
   SDL_JOYHATMOTIONMASK : constant SDL_EventMask := 512;
   SDL_JOYBUTTONDOWNMASK : constant SDL_EventMask := 1024;
   SDL_JOYBUTTONUPMASK : constant SDL_EventMask := 2048;
   SDL_JOYEVENTMASK : constant SDL_EventMask := 3968;
   SDL_VIDEORESIZEMASK : constant SDL_EventMask := 65536;
   SDL_VIDEOEXPOSEMASK : constant SDL_EventMask := 131072;
   SDL_QUITMASK : constant SDL_EventMask := 4096;
   SDL_SYSWMEVENTMASK : constant SDL_EventMask := 8192;  -- ../include/SDL/SDL_events.h:114

   type SDL_ActiveEvent_Rec is record
      c_type : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:120
      gain : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:121
      state : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:122
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_ActiveEvent_Rec);  -- ../include/SDL/SDL_events.h:119

   type SDL_KeyboardEvent is record
      c_type : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:127
      which : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:128
      state : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:129
      keysym : aliased SDL_SDL_keyboard_h.SDL_keysym;  -- ../include/SDL/SDL_events.h:130
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_KeyboardEvent);  -- ../include/SDL/SDL_events.h:126

   type SDL_MouseMotionEvent is record
      c_type : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:135
      which : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:136
      state : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:137
      x : aliased SDL_SDL_stdinc_h.Uint16;  -- ../include/SDL/SDL_events.h:138
      y : aliased SDL_SDL_stdinc_h.Uint16;  -- ../include/SDL/SDL_events.h:138
      xrel : aliased SDL_SDL_stdinc_h.Sint16;  -- ../include/SDL/SDL_events.h:139
      yrel : aliased SDL_SDL_stdinc_h.Sint16;  -- ../include/SDL/SDL_events.h:140
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_MouseMotionEvent);  -- ../include/SDL/SDL_events.h:134

   type SDL_MouseButtonEvent is record
      c_type : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:145
      which : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:146
      button : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:147
      state : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:148
      x : aliased SDL_SDL_stdinc_h.Uint16;  -- ../include/SDL/SDL_events.h:149
      y : aliased SDL_SDL_stdinc_h.Uint16;  -- ../include/SDL/SDL_events.h:149
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_MouseButtonEvent);  -- ../include/SDL/SDL_events.h:144

   type SDL_JoyAxisEvent is record
      c_type : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:154
      which : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:155
      axis : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:156
      value : aliased SDL_SDL_stdinc_h.Sint16;  -- ../include/SDL/SDL_events.h:157
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_JoyAxisEvent);  -- ../include/SDL/SDL_events.h:153

   type SDL_JoyBallEvent is record
      c_type : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:162
      which : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:163
      ball : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:164
      xrel : aliased SDL_SDL_stdinc_h.Sint16;  -- ../include/SDL/SDL_events.h:165
      yrel : aliased SDL_SDL_stdinc_h.Sint16;  -- ../include/SDL/SDL_events.h:166
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_JoyBallEvent);  -- ../include/SDL/SDL_events.h:161

   type SDL_JoyHatEvent is record
      c_type : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:171
      which : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:172
      hat : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:173
      value : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:174
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_JoyHatEvent);  -- ../include/SDL/SDL_events.h:170

   type SDL_JoyButtonEvent is record
      c_type : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:184
      which : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:185
      button : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:186
      state : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:187
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_JoyButtonEvent);  -- ../include/SDL/SDL_events.h:183

   type SDL_ResizeEvent is record
      c_type : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:195
      w : aliased int;  -- ../include/SDL/SDL_events.h:196
      h : aliased int;  -- ../include/SDL/SDL_events.h:197
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_ResizeEvent);  -- ../include/SDL/SDL_events.h:194

   type SDL_ExposeEvent is record
      c_type : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:202
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_ExposeEvent);  -- ../include/SDL/SDL_events.h:201

   type SDL_QuitEvent is record
      c_type : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:207
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_QuitEvent);  -- ../include/SDL/SDL_events.h:206

   type SDL_UserEvent_Rec is record
      c_type : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:212
      code : aliased int;  -- ../include/SDL/SDL_events.h:213
      data1 : System.Address;  -- ../include/SDL/SDL_events.h:214
      data2 : System.Address;  -- ../include/SDL/SDL_events.h:215
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_UserEvent_Rec);  -- ../include/SDL/SDL_events.h:211

   --  skipped empty struct SDL_SysWMmsg

   type SDL_SysWMEvent_Rec is record
      c_type : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:222
      msg : System.Address;  -- ../include/SDL/SDL_events.h:223
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_SysWMEvent_Rec);  -- ../include/SDL/SDL_events.h:221

   type SDL_Event (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            c_type : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:228
         when 1 =>
            active : aliased SDL_ActiveEvent_Rec;  -- ../include/SDL/SDL_events.h:229
         when 2 =>
            key : aliased SDL_KeyboardEvent;  -- ../include/SDL/SDL_events.h:230
         when 3 =>
            motion : aliased SDL_MouseMotionEvent;  -- ../include/SDL/SDL_events.h:231
         when 4 =>
            button : aliased SDL_MouseButtonEvent;  -- ../include/SDL/SDL_events.h:232
         when 5 =>
            jaxis : aliased SDL_JoyAxisEvent;  -- ../include/SDL/SDL_events.h:233
         when 6 =>
            jball : aliased SDL_JoyBallEvent;  -- ../include/SDL/SDL_events.h:234
         when 7 =>
            jhat : aliased SDL_JoyHatEvent;  -- ../include/SDL/SDL_events.h:235
         when 8 =>
            jbutton : aliased SDL_JoyButtonEvent;  -- ../include/SDL/SDL_events.h:236
         when 9 =>
            resize : aliased SDL_ResizeEvent;  -- ../include/SDL/SDL_events.h:237
         when 10 =>
            expose : aliased SDL_ExposeEvent;  -- ../include/SDL/SDL_events.h:238
         when 11 =>
            quit : aliased SDL_QuitEvent;  -- ../include/SDL/SDL_events.h:239
         when 12 =>
            user : aliased SDL_UserEvent_Rec;  -- ../include/SDL/SDL_events.h:240
         when others =>
            syswm : aliased SDL_SysWMEvent_Rec;  -- ../include/SDL/SDL_events.h:241
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_Event);
   pragma Unchecked_Union (SDL_Event);  -- ../include/SDL/SDL_events.h:227

   procedure SDL_PumpEvents;  -- ../include/SDL/SDL_events.h:251
   pragma Import (C, SDL_PumpEvents, "SDL_PumpEvents");

   type SDL_eventaction is 
     (SDL_ADDEVENT,
      SDL_PEEKEVENT,
      SDL_GETEVENT);
   pragma Convention (C, SDL_eventaction);  -- ../include/SDL/SDL_events.h:257

   function SDL_PeepEvents
     (events : access SDL_Event;
      numevents : int;
      action : SDL_eventaction;
      mask : SDL_SDL_stdinc_h.Uint32) return int;  -- ../include/SDL/SDL_events.h:277
   pragma Import (C, SDL_PeepEvents, "SDL_PeepEvents");

   function SDL_PollEvent (event : access SDL_Event) return int;  -- ../include/SDL/SDL_events.h:284
   pragma Import (C, SDL_PollEvent, "SDL_PollEvent");

   function SDL_WaitEvent (event : access SDL_Event) return int;  -- ../include/SDL/SDL_events.h:290
   pragma Import (C, SDL_WaitEvent, "SDL_WaitEvent");

   function SDL_PushEvent (event : access SDL_Event) return int;  -- ../include/SDL/SDL_events.h:296
   pragma Import (C, SDL_PushEvent, "SDL_PushEvent");

   type SDL_EventFilter is access function (arg1 : System.Address) return int;
   pragma Convention (C, SDL_EventFilter);  -- ../include/SDL/SDL_events.h:300

   procedure SDL_SetEventFilter (filter : SDL_EventFilter);  -- ../include/SDL/SDL_events.h:323
   pragma Import (C, SDL_SetEventFilter, "SDL_SetEventFilter");

   function SDL_GetEventFilter return SDL_EventFilter;  -- ../include/SDL/SDL_events.h:329
   pragma Import (C, SDL_GetEventFilter, "SDL_GetEventFilter");

   function SDL_EventState (c_type : SDL_SDL_stdinc_h.Uint8; state : int) return SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_events.h:348
   pragma Import (C, SDL_EventState, "SDL_EventState");

end SDL_SDL_events_h;
