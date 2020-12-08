pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
limited with SDL_video_h;
with SDL_stdinc_h;
limited with SDL_surface_h;

package SDL_mouse_h is

   --  arg-macro: function SDL_BUTTON (X)
   --    return 2 ** ((X)-1);
   SDL_BUTTON_LEFT : constant := 1;  --  ..\SDL2_tmp\SDL_mouse.h:282
   SDL_BUTTON_MIDDLE : constant := 2;  --  ..\SDL2_tmp\SDL_mouse.h:283
   SDL_BUTTON_RIGHT : constant := 3;  --  ..\SDL2_tmp\SDL_mouse.h:284
   SDL_BUTTON_X1 : constant := 4;  --  ..\SDL2_tmp\SDL_mouse.h:285
   SDL_BUTTON_X2 : constant := 5;  --  ..\SDL2_tmp\SDL_mouse.h:286
   --  unsupported macro: SDL_BUTTON_LMASK SDL_BUTTON(SDL_BUTTON_LEFT)
   --  unsupported macro: SDL_BUTTON_MMASK SDL_BUTTON(SDL_BUTTON_MIDDLE)
   --  unsupported macro: SDL_BUTTON_RMASK SDL_BUTTON(SDL_BUTTON_RIGHT)
   --  unsupported macro: SDL_BUTTON_X1MASK SDL_BUTTON(SDL_BUTTON_X1)
   --  unsupported macro: SDL_BUTTON_X2MASK SDL_BUTTON(SDL_BUTTON_X2)

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
  -- *  \file SDL_mouse.h
  -- *
  -- *  Include file for SDL mouse event handling.
  --  

  -- Set up for C function definitions, even when using C++  
  --*< Implementation dependent  
   type SDL_Cursor is null record;   -- incomplete struct

  --*
  -- * \brief Cursor types for SDL_CreateSystemCursor().
  --  

  --*< Arrow  
  --*< I-beam  
  --*< Wait  
  --*< Crosshair  
  --*< Small wait cursor (or Wait if not available)  
  --*< Double arrow pointing northwest and southeast  
  --*< Double arrow pointing northeast and southwest  
  --*< Double arrow pointing west and east  
  --*< Double arrow pointing north and south  
  --*< Four pointed arrow pointing north, south, east, and west  
  --*< Slashed circle or crossbones  
  --*< Hand  
   type SDL_SystemCursor is 
     (SDL_SYSTEM_CURSOR_ARROW,
      SDL_SYSTEM_CURSOR_IBEAM,
      SDL_SYSTEM_CURSOR_WAIT,
      SDL_SYSTEM_CURSOR_CROSSHAIR,
      SDL_SYSTEM_CURSOR_WAITARROW,
      SDL_SYSTEM_CURSOR_SIZENWSE,
      SDL_SYSTEM_CURSOR_SIZENESW,
      SDL_SYSTEM_CURSOR_SIZEWE,
      SDL_SYSTEM_CURSOR_SIZENS,
      SDL_SYSTEM_CURSOR_SIZEALL,
      SDL_SYSTEM_CURSOR_NO,
      SDL_SYSTEM_CURSOR_HAND,
      SDL_NUM_SYSTEM_CURSORS);
   pragma Convention (C, SDL_SystemCursor);  -- ..\SDL2_tmp\SDL_mouse.h:61

  --*
  -- * \brief Scroll direction types for the Scroll event
  --  

  --*< The scroll direction is normal  
  --*< The scroll direction is flipped / natural  
   type SDL_MouseWheelDirection is 
     (SDL_MOUSEWHEEL_NORMAL,
      SDL_MOUSEWHEEL_FLIPPED);
   pragma Convention (C, SDL_MouseWheelDirection);  -- ..\SDL2_tmp\SDL_mouse.h:70

  -- Function prototypes  
  --*
  -- *  \brief Get the window which currently has mouse focus.
  --  

   function SDL_GetMouseFocus return access SDL_video_h.Class_SDL_Window.SDL_Window;  -- ..\SDL2_tmp\SDL_mouse.h:77
   pragma Import (C, SDL_GetMouseFocus, "SDL_GetMouseFocus");

  --*
  -- *  \brief Retrieve the current state of the mouse.
  -- *
  -- *  The current button state is returned as a button bitmask, which can
  -- *  be tested using the SDL_BUTTON(X) macros, and x and y are set to the
  -- *  mouse cursor position relative to the focus window for the currently
  -- *  selected mouse.  You can pass NULL for either x or y.
  --  

   function SDL_GetMouseState (x : access int; y : access int) return SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_mouse.h:87
   pragma Import (C, SDL_GetMouseState, "SDL_GetMouseState");

  --*
  -- *  \brief Get the current state of the mouse, in relation to the desktop
  -- *
  -- *  This works just like SDL_GetMouseState(), but the coordinates will be
  -- *  reported relative to the top-left of the desktop. This can be useful if
  -- *  you need to track the mouse outside of a specific window and
  -- *  SDL_CaptureMouse() doesn't fit your needs. For example, it could be
  -- *  useful if you need to track the mouse while dragging a window, where
  -- *  coordinates relative to a window might not be in sync at all times.
  -- *
  -- *  \note SDL_GetMouseState() returns the mouse position as SDL understands
  -- *        it from the last pump of the event queue. This function, however,
  -- *        queries the OS for the current mouse position, and as such, might
  -- *        be a slightly less efficient function. Unless you know what you're
  -- *        doing and have a good reason to use this function, you probably want
  -- *        SDL_GetMouseState() instead.
  -- *
  -- *  \param x Returns the current X coord, relative to the desktop. Can be NULL.
  -- *  \param y Returns the current Y coord, relative to the desktop. Can be NULL.
  -- *  \return The current button state as a bitmask, which can be tested using the SDL_BUTTON(X) macros.
  -- *
  -- *  \sa SDL_GetMouseState
  --  

   function SDL_GetGlobalMouseState (x : access int; y : access int) return SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_mouse.h:112
   pragma Import (C, SDL_GetGlobalMouseState, "SDL_GetGlobalMouseState");

  --*
  -- *  \brief Retrieve the relative state of the mouse.
  -- *
  -- *  The current button state is returned as a button bitmask, which can
  -- *  be tested using the SDL_BUTTON(X) macros, and x and y are set to the
  -- *  mouse deltas since the last call to SDL_GetRelativeMouseState().
  --  

   function SDL_GetRelativeMouseState (x : access int; y : access int) return SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_mouse.h:121
   pragma Import (C, SDL_GetRelativeMouseState, "SDL_GetRelativeMouseState");

  --*
  -- *  \brief Moves the mouse to the given position within the window.
  -- *
  -- *  \param window The window to move the mouse into, or NULL for the current mouse focus
  -- *  \param x The x coordinate within the window
  -- *  \param y The y coordinate within the window
  -- *
  -- *  \note This function generates a mouse motion event
  --  

   procedure SDL_WarpMouseInWindow
     (window : access SDL_video_h.Class_SDL_Window.SDL_Window;
      x : int;
      y : int);  -- ..\SDL2_tmp\SDL_mouse.h:132
   pragma Import (C, SDL_WarpMouseInWindow, "SDL_WarpMouseInWindow");

  --*
  -- *  \brief Moves the mouse to the given position in global screen space.
  -- *
  -- *  \param x The x coordinate
  -- *  \param y The y coordinate
  -- *  \return 0 on success, -1 on error (usually: unsupported by a platform).
  -- *
  -- *  \note This function generates a mouse motion event
  --  

   function SDL_WarpMouseGlobal (x : int; y : int) return int;  -- ..\SDL2_tmp\SDL_mouse.h:144
   pragma Import (C, SDL_WarpMouseGlobal, "SDL_WarpMouseGlobal");

  --*
  -- *  \brief Set relative mouse mode.
  -- *
  -- *  \param enabled Whether or not to enable relative mode
  -- *
  -- *  \return 0 on success, or -1 if relative mode is not supported.
  -- *
  -- *  While the mouse is in relative mode, the cursor is hidden, and the
  -- *  driver will try to report continuous motion in the current window.
  -- *  Only relative motion events will be delivered, the mouse position
  -- *  will not change.
  -- *
  -- *  \note This function will flush any pending mouse motion.
  -- *
  -- *  \sa SDL_GetRelativeMouseMode()
  --  

   function SDL_SetRelativeMouseMode (enabled : SDL_stdinc_h.SDL_bool) return int;  -- ..\SDL2_tmp\SDL_mouse.h:162
   pragma Import (C, SDL_SetRelativeMouseMode, "SDL_SetRelativeMouseMode");

  --*
  -- *  \brief Capture the mouse, to track input outside an SDL window.
  -- *
  -- *  \param enabled Whether or not to enable capturing
  -- *
  -- *  Capturing enables your app to obtain mouse events globally, instead of
  -- *  just within your window. Not all video targets support this function.
  -- *  When capturing is enabled, the current window will get all mouse events,
  -- *  but unlike relative mode, no change is made to the cursor and it is
  -- *  not restrained to your window.
  -- *
  -- *  This function may also deny mouse input to other windows--both those in
  -- *  your application and others on the system--so you should use this
  -- *  function sparingly, and in small bursts. For example, you might want to
  -- *  track the mouse while the user is dragging something, until the user
  -- *  releases a mouse button. It is not recommended that you capture the mouse
  -- *  for long periods of time, such as the entire time your app is running.
  -- *
  -- *  While captured, mouse events still report coordinates relative to the
  -- *  current (foreground) window, but those coordinates may be outside the
  -- *  bounds of the window (including negative values). Capturing is only
  -- *  allowed for the foreground window. If the window loses focus while
  -- *  capturing, the capture will be disabled automatically.
  -- *
  -- *  While capturing is enabled, the current window will have the
  -- *  SDL_WINDOW_MOUSE_CAPTURE flag set.
  -- *
  -- *  \return 0 on success, or -1 if not supported.
  --  

   function SDL_CaptureMouse (enabled : SDL_stdinc_h.SDL_bool) return int;  -- ..\SDL2_tmp\SDL_mouse.h:193
   pragma Import (C, SDL_CaptureMouse, "SDL_CaptureMouse");

  --*
  -- *  \brief Query whether relative mouse mode is enabled.
  -- *
  -- *  \sa SDL_SetRelativeMouseMode()
  --  

   function SDL_GetRelativeMouseMode return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_mouse.h:200
   pragma Import (C, SDL_GetRelativeMouseMode, "SDL_GetRelativeMouseMode");

  --*
  -- *  \brief Create a cursor, using the specified bitmap data and
  -- *         mask (in MSB format).
  -- *
  -- *  The cursor width must be a multiple of 8 bits.
  -- *
  -- *  The cursor is created in black and white according to the following:
  -- *  <table>
  -- *  <tr><td> data </td><td> mask </td><td> resulting pixel on screen </td></tr>
  -- *  <tr><td>  0   </td><td>  1   </td><td> White </td></tr>
  -- *  <tr><td>  1   </td><td>  1   </td><td> Black </td></tr>
  -- *  <tr><td>  0   </td><td>  0   </td><td> Transparent </td></tr>
  -- *  <tr><td>  1   </td><td>  0   </td><td> Inverted color if possible, black
  -- *                                         if not. </td></tr>
  -- *  </table>
  -- *
  -- *  \sa SDL_FreeCursor()
  --  

   function SDL_CreateCursor
     (data : access SDL_stdinc_h.Uint8;
      mask : access SDL_stdinc_h.Uint8;
      w : int;
      h : int;
      hot_x : int;
      hot_y : int) return access SDL_Cursor;  -- ..\SDL2_tmp\SDL_mouse.h:220
   pragma Import (C, SDL_CreateCursor, "SDL_CreateCursor");

  --*
  -- *  \brief Create a color cursor.
  -- *
  -- *  \sa SDL_FreeCursor()
  --  

   function SDL_CreateColorCursor
     (surface : access SDL_surface_h.SDL_Surface;
      hot_x : int;
      hot_y : int) return access SDL_Cursor;  -- ..\SDL2_tmp\SDL_mouse.h:230
   pragma Import (C, SDL_CreateColorCursor, "SDL_CreateColorCursor");

  --*
  -- *  \brief Create a system cursor.
  -- *
  -- *  \sa SDL_FreeCursor()
  --  

   function SDL_CreateSystemCursor (id : SDL_SystemCursor) return access SDL_Cursor;  -- ..\SDL2_tmp\SDL_mouse.h:239
   pragma Import (C, SDL_CreateSystemCursor, "SDL_CreateSystemCursor");

  --*
  -- *  \brief Set the active cursor.
  --  

   procedure SDL_SetCursor (cursor : access SDL_Cursor);  -- ..\SDL2_tmp\SDL_mouse.h:244
   pragma Import (C, SDL_SetCursor, "SDL_SetCursor");

  --*
  -- *  \brief Return the active cursor.
  --  

   function SDL_GetCursor return access SDL_Cursor;  -- ..\SDL2_tmp\SDL_mouse.h:249
   pragma Import (C, SDL_GetCursor, "SDL_GetCursor");

  --*
  -- *  \brief Return the default cursor.
  --  

   function SDL_GetDefaultCursor return access SDL_Cursor;  -- ..\SDL2_tmp\SDL_mouse.h:254
   pragma Import (C, SDL_GetDefaultCursor, "SDL_GetDefaultCursor");

  --*
  -- *  \brief Frees a cursor created with SDL_CreateCursor() or similar functions.
  -- *
  -- *  \sa SDL_CreateCursor()
  -- *  \sa SDL_CreateColorCursor()
  -- *  \sa SDL_CreateSystemCursor()
  --  

   procedure SDL_FreeCursor (cursor : access SDL_Cursor);  -- ..\SDL2_tmp\SDL_mouse.h:263
   pragma Import (C, SDL_FreeCursor, "SDL_FreeCursor");

  --*
  -- *  \brief Toggle whether or not the cursor is shown.
  -- *
  -- *  \param toggle 1 to show the cursor, 0 to hide it, -1 to query the current
  -- *                state.
  -- *
  -- *  \return 1 if the cursor is shown, or 0 if the cursor is hidden.
  --  

   function SDL_ShowCursor (toggle : int) return int;  -- ..\SDL2_tmp\SDL_mouse.h:273
   pragma Import (C, SDL_ShowCursor, "SDL_ShowCursor");

  --*
  -- *  Used as a mask when testing buttons in buttonstate.
  -- *   - Button 1:  Left mouse button
  -- *   - Button 2:  Middle mouse button
  -- *   - Button 3:  Right mouse button
  --  

  -- Ends C function definitions when using C++  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_mouse_h;
