pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with SDL_stdinc_h;
limited with SDL_video_h;
with SDL_pixels_h;
limited with SDL_surface_h;

package SDL_shape_h is

   SDL_NONSHAPEABLE_WINDOW : constant := -1;  --  ..\SDL2_tmp\SDL_shape.h:42
   SDL_INVALID_SHAPE_ARGUMENT : constant := -2;  --  ..\SDL2_tmp\SDL_shape.h:43
   SDL_WINDOW_LACKS_SHAPE : constant := -3;  --  ..\SDL2_tmp\SDL_shape.h:44
   --  arg-macro: function SDL_SHAPEMODEALPHA (mode)
   --    return mode = ShapeModeDefault  or else  mode = ShapeModeBinarizeAlpha  or else  mode = ShapeModeReverseBinarizeAlpha;

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

  -- Set up for C function definitions, even when using C++  
  --* \file SDL_shape.h
  -- *
  -- * Header file for the shaped window API.
  --  

  --*
  -- *  \brief Create a window that can be shaped with the specified position, dimensions, and flags.
  -- *
  -- *  \param title The title of the window, in UTF-8 encoding.
  -- *  \param x     The x position of the window, ::SDL_WINDOWPOS_CENTERED, or
  -- *               ::SDL_WINDOWPOS_UNDEFINED.
  -- *  \param y     The y position of the window, ::SDL_WINDOWPOS_CENTERED, or
  -- *               ::SDL_WINDOWPOS_UNDEFINED.
  -- *  \param w     The width of the window.
  -- *  \param h     The height of the window.
  -- *  \param flags The flags for the window, a mask of SDL_WINDOW_BORDERLESS with any of the following:
  -- *               ::SDL_WINDOW_OPENGL,     ::SDL_WINDOW_INPUT_GRABBED,
  -- *               ::SDL_WINDOW_HIDDEN,     ::SDL_WINDOW_RESIZABLE,
  -- *               ::SDL_WINDOW_MAXIMIZED,  ::SDL_WINDOW_MINIMIZED,
  -- *       ::SDL_WINDOW_BORDERLESS is always set, and ::SDL_WINDOW_FULLSCREEN is always unset.
  -- *
  -- *  \return The window created, or NULL if window creation failed.
  -- *
  -- *  \sa SDL_DestroyWindow()
  --  

   function SDL_CreateShapedWindow
     (title : Interfaces.C.Strings.chars_ptr;
      x : unsigned;
      y : unsigned;
      w : unsigned;
      h : unsigned;
      flags : SDL_stdinc_h.Uint32) return access SDL_video_h.Class_SDL_Window.SDL_Window;  -- ..\SDL2_tmp\SDL_shape.h:66
   pragma Import (C, SDL_CreateShapedWindow, "SDL_CreateShapedWindow");

  --*
  -- * \brief Return whether the given window is a shaped window.
  -- *
  -- * \param window The window to query for being shaped.
  -- *
  -- * \return SDL_TRUE if the window is a window that can be shaped, SDL_FALSE if the window is unshaped or NULL.
  -- *
  -- * \sa SDL_CreateShapedWindow
  --  

   function SDL_IsShapedWindow (window : access constant SDL_video_h.Class_SDL_Window.SDL_Window) return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_shape.h:77
   pragma Import (C, SDL_IsShapedWindow, "SDL_IsShapedWindow");

  --* \brief An enum denoting the specific type of contents present in an SDL_WindowShapeParams union.  
  --* \brief The default mode, a binarized alpha cutoff of 1.  
  --* \brief A binarized alpha cutoff with a given integer value.  
  --* \brief A binarized alpha cutoff with a given integer value, but with the opposite comparison.  
  --* \brief A color key is applied.  
   type WindowShapeMode is 
     (ShapeModeDefault,
      ShapeModeBinarizeAlpha,
      ShapeModeReverseBinarizeAlpha,
      ShapeModeColorKey);
   pragma Convention (C, WindowShapeMode);  -- ..\SDL2_tmp\SDL_shape.h:89

  --* \brief A union containing parameters for shaped windows.  
  --* \brief A cutoff alpha value for binarization of the window shape's alpha channel.  
   type SDL_WindowShapeParams (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            binarizationCutoff : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_shape.h:96
         when others =>
            colorKey : aliased SDL_pixels_h.SDL_Color;  -- ..\SDL2_tmp\SDL_shape.h:97
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_WindowShapeParams);
   pragma Unchecked_Union (SDL_WindowShapeParams);  -- ..\SDL2_tmp\SDL_shape.h:98

   --  skipped anonymous struct anon_80

  --* \brief A struct that tags the SDL_WindowShapeParams union with an enum describing the type of its contents.  
  --* \brief The mode of these window-shape parameters.  
   type SDL_WindowShapeMode is record
      mode : aliased WindowShapeMode;  -- ..\SDL2_tmp\SDL_shape.h:103
      parameters : aliased SDL_WindowShapeParams;  -- ..\SDL2_tmp\SDL_shape.h:105
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_WindowShapeMode);  -- ..\SDL2_tmp\SDL_shape.h:101

  --* \brief Window-shape parameters.  
  --*
  -- * \brief Set the shape and parameters of a shaped window.
  -- *
  -- * \param window The shaped window whose parameters should be set.
  -- * \param shape A surface encoding the desired shape for the window.
  -- * \param shape_mode The parameters to set for the shaped window.
  -- *
  -- * \return 0 on success, SDL_INVALID_SHAPE_ARGUMENT on an invalid shape argument, or SDL_NONSHAPEABLE_WINDOW
  -- *           if the SDL_Window given does not reference a valid shaped window.
  -- *
  -- * \sa SDL_WindowShapeMode
  -- * \sa SDL_GetShapedWindowMode.
  --  

   function SDL_SetWindowShape
     (window : access SDL_video_h.Class_SDL_Window.SDL_Window;
      shape : access SDL_surface_h.SDL_Surface;
      shape_mode : access SDL_WindowShapeMode) return int;  -- ..\SDL2_tmp\SDL_shape.h:121
   pragma Import (C, SDL_SetWindowShape, "SDL_SetWindowShape");

  --*
  -- * \brief Get the shape parameters of a shaped window.
  -- *
  -- * \param window The shaped window whose parameters should be retrieved.
  -- * \param shape_mode An empty shape-mode structure to fill, or NULL to check whether the window has a shape.
  -- *
  -- * \return 0 if the window has a shape and, provided shape_mode was not NULL, shape_mode has been filled with the mode
  -- *           data, SDL_NONSHAPEABLE_WINDOW if the SDL_Window given is not a shaped window, or SDL_WINDOW_LACKS_SHAPE if
  -- *           the SDL_Window given is a shapeable window currently lacking a shape.
  -- *
  -- * \sa SDL_WindowShapeMode
  -- * \sa SDL_SetWindowShape
  --  

   function SDL_GetShapedWindowMode (window : access SDL_video_h.Class_SDL_Window.SDL_Window; shape_mode : access SDL_WindowShapeMode) return int;  -- ..\SDL2_tmp\SDL_shape.h:136
   pragma Import (C, SDL_GetShapedWindowMode, "SDL_GetShapedWindowMode");

  -- Ends C function definitions when using C++  
end SDL_shape_h;
