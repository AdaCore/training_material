pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_SDL_stdinc_h;
with SDL_SDL_video_h;
with System;

package SDL_SDL_mouse_h is

   --  arg-macro: function SDL_BUTTON (X)
   --    return 2 ** ((X)-1);

   SDL_BUTTON_LEFT : constant := 1;  --  ../include/SDL/SDL_mouse.h:123
   SDL_BUTTON_MIDDLE : constant := 2;  --  ../include/SDL/SDL_mouse.h:124
   SDL_BUTTON_RIGHT : constant := 3;  --  ../include/SDL/SDL_mouse.h:125
   SDL_BUTTON_WHEELUP : constant := 4;  --  ../include/SDL/SDL_mouse.h:126
   SDL_BUTTON_WHEELDOWN : constant := 5;  --  ../include/SDL/SDL_mouse.h:127
   SDL_BUTTON_X1 : constant := 6;  --  ../include/SDL/SDL_mouse.h:128
   SDL_BUTTON_X2 : constant := 7;  --  ../include/SDL/SDL_mouse.h:129
   --  unsupported macro: SDL_BUTTON_LMASK SDL_BUTTON(SDL_BUTTON_LEFT)
   --  unsupported macro: SDL_BUTTON_MMASK SDL_BUTTON(SDL_BUTTON_MIDDLE)
   --  unsupported macro: SDL_BUTTON_RMASK SDL_BUTTON(SDL_BUTTON_RIGHT)
   --  unsupported macro: SDL_BUTTON_X1MASK SDL_BUTTON(SDL_BUTTON_X1)
   --  unsupported macro: SDL_BUTTON_X2MASK SDL_BUTTON(SDL_BUTTON_X2)

   --  skipped empty struct WMcursor

   type SDL_Cursor_save_array is array (0 .. 1) of access SDL_SDL_stdinc_h.Uint8;
   type SDL_Cursor is record
      area : aliased SDL_SDL_video_h.SDL_Rect;  -- ../include/SDL/SDL_mouse.h:42
      hot_x : aliased SDL_SDL_stdinc_h.Sint16;  -- ../include/SDL/SDL_mouse.h:43
      hot_y : aliased SDL_SDL_stdinc_h.Sint16;  -- ../include/SDL/SDL_mouse.h:43
      data : access SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_mouse.h:44
      mask : access SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_mouse.h:45
      save : aliased SDL_Cursor_save_array;  -- ../include/SDL/SDL_mouse.h:46
      wm_cursor : System.Address;  -- ../include/SDL/SDL_mouse.h:47
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_Cursor);  -- ../include/SDL/SDL_mouse.h:41

   function SDL_GetMouseState (x : access int; y : access int) return SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_mouse.h:57
   pragma Import (C, SDL_GetMouseState, "SDL_GetMouseState");

   function SDL_GetRelativeMouseState (x : access int; y : access int) return SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_mouse.h:65
   pragma Import (C, SDL_GetRelativeMouseState, "SDL_GetRelativeMouseState");

   procedure SDL_WarpMouse (x : SDL_SDL_stdinc_h.Uint16; y : SDL_SDL_stdinc_h.Uint16);  -- ../include/SDL/SDL_mouse.h:70
   pragma Import (C, SDL_WarpMouse, "SDL_WarpMouse");

   function SDL_CreateCursor
     (data : access SDL_SDL_stdinc_h.Uint8;
      mask : access SDL_SDL_stdinc_h.Uint8;
      w : int;
      h : int;
      hot_x : int;
      hot_y : int) return access SDL_Cursor;  -- ../include/SDL/SDL_mouse.h:85
   pragma Import (C, SDL_CreateCursor, "SDL_CreateCursor");

   procedure SDL_SetCursor (cursor : access SDL_Cursor);  -- ../include/SDL/SDL_mouse.h:93
   pragma Import (C, SDL_SetCursor, "SDL_SetCursor");

   function SDL_GetCursor return access SDL_Cursor;  -- ../include/SDL/SDL_mouse.h:98
   pragma Import (C, SDL_GetCursor, "SDL_GetCursor");

   procedure SDL_FreeCursor (cursor : access SDL_Cursor);  -- ../include/SDL/SDL_mouse.h:103
   pragma Import (C, SDL_FreeCursor, "SDL_FreeCursor");

   function SDL_ShowCursor (toggle : int) return int;  -- ../include/SDL/SDL_mouse.h:112
   pragma Import (C, SDL_ShowCursor, "SDL_ShowCursor");

end SDL_SDL_mouse_h;
