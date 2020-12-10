pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_stdinc_h;
with System;
with Interfaces.C.Strings;
limited with SDL_rect_h;
limited with SDL_surface_h;

package SDL_video_h is

   SDL_WINDOWPOS_UNDEFINED_MASK : constant := 16#1FFF0000#;  --  ..\SDL2_tmp\SDL_video.h:128
   --  arg-macro: function SDL_WINDOWPOS_UNDEFINED_DISPLAY (X)
   --    return SDL_WINDOWPOS_UNDEFINED_MASKor(X);
   --  unsupported macro: SDL_WINDOWPOS_UNDEFINED SDL_WINDOWPOS_UNDEFINED_DISPLAY(0)
   --  arg-macro: function SDL_WINDOWPOS_ISUNDEFINED (X)
   --    return ((X)and16#FFFF0000#) = SDL_WINDOWPOS_UNDEFINED_MASK;

   --  manual fix for Ada - will only work for display 0
   SDL_WINDOWPOS_UNDEFINED : constant := SDL_WINDOWPOS_UNDEFINED_MASK;                                                     
   
   
   SDL_WINDOWPOS_CENTERED_MASK : constant := 16#2FFF0000#;  --  ..\SDL2_tmp\SDL_video.h:137
   --  arg-macro: function SDL_WINDOWPOS_CENTERED_DISPLAY (X)
   --    return SDL_WINDOWPOS_CENTERED_MASKor(X);
   --  unsupported macro: SDL_WINDOWPOS_CENTERED SDL_WINDOWPOS_CENTERED_DISPLAY(0)
   --  arg-macro: function SDL_WINDOWPOS_ISCENTERED (X)
   --    return ((X)and16#FFFF0000#) = SDL_WINDOWPOS_CENTERED_MASK;
 
   --  manual fix for Ada - will only work for display 0
   SDL_WINDOWPOS_CENTERED : constant := SDL_WINDOWPOS_CENTERED_MASK;                                                     
 
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
  -- *  \file SDL_video.h
  -- *
  -- *  Header file for SDL video functions.
  --  

  -- Set up for C function definitions, even when using C++  
  --*
  -- *  \brief  The structure that defines a display mode
  -- *
  -- *  \sa SDL_GetNumDisplayModes()
  -- *  \sa SDL_GetDisplayMode()
  -- *  \sa SDL_GetDesktopDisplayMode()
  -- *  \sa SDL_GetCurrentDisplayMode()
  -- *  \sa SDL_GetClosestDisplayMode()
  -- *  \sa SDL_SetWindowDisplayMode()
  -- *  \sa SDL_GetWindowDisplayMode()
  --  

  --*< pixel format  
   type SDL_DisplayMode is record
      format : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_video.h:55
      w : aliased int;  -- ..\SDL2_tmp\SDL_video.h:56
      h : aliased int;  -- ..\SDL2_tmp\SDL_video.h:57
      refresh_rate : aliased int;  -- ..\SDL2_tmp\SDL_video.h:58
      driverdata : System.Address;  -- ..\SDL2_tmp\SDL_video.h:59
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_DisplayMode);  -- ..\SDL2_tmp\SDL_video.h:60

   --  skipped anonymous struct anon_37

  --*< width, in screen coordinates  
  --*< height, in screen coordinates  
  --*< refresh rate (or zero for unspecified)  
  --*< driver-specific data, initialize to 0  
  --*
  -- *  \brief The type used to identify a window
  -- *
  -- *  \sa SDL_CreateWindow()
  -- *  \sa SDL_CreateWindowFrom()
  -- *  \sa SDL_DestroyWindow()
  -- *  \sa SDL_GetWindowData()
  -- *  \sa SDL_GetWindowFlags()
  -- *  \sa SDL_GetWindowGrab()
  -- *  \sa SDL_GetWindowPosition()
  -- *  \sa SDL_GetWindowSize()
  -- *  \sa SDL_GetWindowTitle()
  -- *  \sa SDL_HideWindow()
  -- *  \sa SDL_MaximizeWindow()
  -- *  \sa SDL_MinimizeWindow()
  -- *  \sa SDL_RaiseWindow()
  -- *  \sa SDL_RestoreWindow()
  -- *  \sa SDL_SetWindowData()
  -- *  \sa SDL_SetWindowFullscreen()
  -- *  \sa SDL_SetWindowGrab()
  -- *  \sa SDL_SetWindowIcon()
  -- *  \sa SDL_SetWindowPosition()
  -- *  \sa SDL_SetWindowSize()
  -- *  \sa SDL_SetWindowBordered()
  -- *  \sa SDL_SetWindowResizable()
  -- *  \sa SDL_SetWindowTitle()
  -- *  \sa SDL_ShowWindow()
  --  

   type SDL_Window is null record;   -- incomplete struct

  --*
  -- *  \brief The flags on a window
  -- *
  -- *  \sa SDL_GetWindowFlags()
  --  

  -- !!! FIXME: change this to name = (1<<x).  
  --*< fullscreen window  
  --*< window usable with OpenGL context  
  --*< window is visible  
  --*< window is not visible  
  --*< no window decoration  
  --*< window can be resized  
  --*< window is minimized  
  --*< window is maximized  
  --*< window has grabbed input focus  
  --*< window has input focus  
  --*< window has mouse focus  
  --*< window not created by SDL  
  --*< window should be created in high-DPI mode if supported.
  --                                                     On macOS NSHighResolutionCapable must be set true in the
  --                                                     application's Info.plist for this to have any effect.  

  --*< window has mouse captured (unrelated to INPUT_GRABBED)  
  --*< window should always be above others  
  --*< window should not be added to the taskbar  
  --*< window should be treated as a utility window  
  --*< window should be treated as a tooltip  
  --*< window should be treated as a popup menu  
  --*< window usable for Vulkan surface  
   subtype SDL_WindowFlags is unsigned;
   SDL_WINDOW_FULLSCREEN : constant unsigned := 1;
   SDL_WINDOW_OPENGL : constant unsigned := 2;
   SDL_WINDOW_SHOWN : constant unsigned := 4;
   SDL_WINDOW_HIDDEN : constant unsigned := 8;
   SDL_WINDOW_BORDERLESS : constant unsigned := 16;
   SDL_WINDOW_RESIZABLE : constant unsigned := 32;
   SDL_WINDOW_MINIMIZED : constant unsigned := 64;
   SDL_WINDOW_MAXIMIZED : constant unsigned := 128;
   SDL_WINDOW_INPUT_GRABBED : constant unsigned := 256;
   SDL_WINDOW_INPUT_FOCUS : constant unsigned := 512;
   SDL_WINDOW_MOUSE_FOCUS : constant unsigned := 1024;
   SDL_WINDOW_FULLSCREEN_DESKTOP : constant unsigned := 4097;
   SDL_WINDOW_FOREIGN : constant unsigned := 2048;
   SDL_WINDOW_ALLOW_HIGHDPI : constant unsigned := 8192;
   SDL_WINDOW_MOUSE_CAPTURE : constant unsigned := 16384;
   SDL_WINDOW_ALWAYS_ON_TOP : constant unsigned := 32768;
   SDL_WINDOW_SKIP_TASKBAR : constant unsigned := 65536;
   SDL_WINDOW_UTILITY : constant unsigned := 131072;
   SDL_WINDOW_TOOLTIP : constant unsigned := 262144;
   SDL_WINDOW_POPUP_MENU : constant unsigned := 524288;
   SDL_WINDOW_VULKAN : constant unsigned := 268435456;  -- ..\SDL2_tmp\SDL_video.h:123

  --*
  -- *  \brief Used to indicate that you don't care what the window position is.
  --  

  --*
  -- *  \brief Used to indicate that the window position should be centered.
  --  

  --*
  -- *  \brief Event subtype for window events
  --  

  --*< Never used  
  --*< Window has been shown  
  --*< Window has been hidden  
  --*< Window has been exposed and should be
  --                                         redrawn  

  --*< Window has been moved to data1, data2
  --                                      

  --*< Window has been resized to data1xdata2  
  --*< The window size has changed, either as
  --                                         a result of an API call or through the
  --                                         system or user changing the window size.  

  --*< Window has been minimized  
  --*< Window has been maximized  
  --*< Window has been restored to normal size
  --                                         and position  

  --*< Window has gained mouse focus  
  --*< Window has lost mouse focus  
  --*< Window has gained keyboard focus  
  --*< Window has lost keyboard focus  
  --*< The window manager requests that the window be closed  
  --*< Window is being offered a focus (should SetWindowInputFocus() on itself or a subwindow, or ignore)  
  --*< Window had a hit test that wasn't SDL_HITTEST_NORMAL.  
   type SDL_WindowEventID is 
     (SDL_WINDOWEVENT_NONE,
      SDL_WINDOWEVENT_SHOWN,
      SDL_WINDOWEVENT_HIDDEN,
      SDL_WINDOWEVENT_EXPOSED,
      SDL_WINDOWEVENT_MOVED,
      SDL_WINDOWEVENT_RESIZED,
      SDL_WINDOWEVENT_SIZE_CHANGED,
      SDL_WINDOWEVENT_MINIMIZED,
      SDL_WINDOWEVENT_MAXIMIZED,
      SDL_WINDOWEVENT_RESTORED,
      SDL_WINDOWEVENT_ENTER,
      SDL_WINDOWEVENT_LEAVE,
      SDL_WINDOWEVENT_FOCUS_GAINED,
      SDL_WINDOWEVENT_FOCUS_LOST,
      SDL_WINDOWEVENT_CLOSE,
      SDL_WINDOWEVENT_TAKE_FOCUS,
      SDL_WINDOWEVENT_HIT_TEST);
   pragma Convention (C, SDL_WindowEventID);  -- ..\SDL2_tmp\SDL_video.h:170

  --*
  -- *  \brief Event subtype for display events
  --  

  --*< Never used  
  --*< Display orientation has changed to data1  
   type SDL_DisplayEventID is 
     (SDL_DISPLAYEVENT_NONE,
      SDL_DISPLAYEVENT_ORIENTATION);
   pragma Convention (C, SDL_DisplayEventID);  -- ..\SDL2_tmp\SDL_video.h:179

  --*< The display orientation can't be determined  
  --*< The display is in landscape mode, with the right side up, relative to portrait mode  
  --*< The display is in landscape mode, with the left side up, relative to portrait mode  
  --*< The display is in portrait mode  
  --*< The display is in portrait mode, upside down  
   type SDL_DisplayOrientation is 
     (SDL_ORIENTATION_UNKNOWN,
      SDL_ORIENTATION_LANDSCAPE,
      SDL_ORIENTATION_LANDSCAPE_FLIPPED,
      SDL_ORIENTATION_PORTRAIT,
      SDL_ORIENTATION_PORTRAIT_FLIPPED);
   pragma Convention (C, SDL_DisplayOrientation);  -- ..\SDL2_tmp\SDL_video.h:188

  --*
  -- *  \brief An opaque handle to an OpenGL context.
  --  

   type SDL_GLContext is new System.Address;  -- ..\SDL2_tmp\SDL_video.h:193

  --*
  -- *  \brief OpenGL configuration attributes
  --  

   type SDL_GLattr is 
     (SDL_GL_RED_SIZE,
      SDL_GL_GREEN_SIZE,
      SDL_GL_BLUE_SIZE,
      SDL_GL_ALPHA_SIZE,
      SDL_GL_BUFFER_SIZE,
      SDL_GL_DOUBLEBUFFER,
      SDL_GL_DEPTH_SIZE,
      SDL_GL_STENCIL_SIZE,
      SDL_GL_ACCUM_RED_SIZE,
      SDL_GL_ACCUM_GREEN_SIZE,
      SDL_GL_ACCUM_BLUE_SIZE,
      SDL_GL_ACCUM_ALPHA_SIZE,
      SDL_GL_STEREO,
      SDL_GL_MULTISAMPLEBUFFERS,
      SDL_GL_MULTISAMPLESAMPLES,
      SDL_GL_ACCELERATED_VISUAL,
      SDL_GL_RETAINED_BACKING,
      SDL_GL_CONTEXT_MAJOR_VERSION,
      SDL_GL_CONTEXT_MINOR_VERSION,
      SDL_GL_CONTEXT_EGL,
      SDL_GL_CONTEXT_FLAGS,
      SDL_GL_CONTEXT_PROFILE_MASK,
      SDL_GL_SHARE_WITH_CURRENT_CONTEXT,
      SDL_GL_FRAMEBUFFER_SRGB_CAPABLE,
      SDL_GL_CONTEXT_RELEASE_BEHAVIOR,
      SDL_GL_CONTEXT_RESET_NOTIFICATION,
      SDL_GL_CONTEXT_NO_ERROR);
   pragma Convention (C, SDL_GLattr);  -- ..\SDL2_tmp\SDL_video.h:227

  --*< GLX_CONTEXT_ES2_PROFILE_BIT_EXT  
   subtype SDL_GLprofile is unsigned;
   SDL_GL_CONTEXT_PROFILE_CORE : constant unsigned := 1;
   SDL_GL_CONTEXT_PROFILE_COMPATIBILITY : constant unsigned := 2;
   SDL_GL_CONTEXT_PROFILE_ES : constant unsigned := 4;  -- ..\SDL2_tmp\SDL_video.h:234

   subtype SDL_GLcontextFlag is unsigned;
   SDL_GL_CONTEXT_DEBUG_FLAG : constant unsigned := 1;
   SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG : constant unsigned := 2;
   SDL_GL_CONTEXT_ROBUST_ACCESS_FLAG : constant unsigned := 4;
   SDL_GL_CONTEXT_RESET_ISOLATION_FLAG : constant unsigned := 8;  -- ..\SDL2_tmp\SDL_video.h:242

   type SDL_GLcontextReleaseFlag is 
     (SDL_GL_CONTEXT_RELEASE_BEHAVIOR_NONE,
      SDL_GL_CONTEXT_RELEASE_BEHAVIOR_FLUSH);
   pragma Convention (C, SDL_GLcontextReleaseFlag);  -- ..\SDL2_tmp\SDL_video.h:248

   type SDL_GLContextResetNotification is 
     (SDL_GL_CONTEXT_RESET_NO_NOTIFICATION,
      SDL_GL_CONTEXT_RESET_LOSE_CONTEXT);
   pragma Convention (C, SDL_GLContextResetNotification);  -- ..\SDL2_tmp\SDL_video.h:254

  -- Function prototypes  
  --*
  -- *  \brief Get the number of video drivers compiled into SDL
  -- *
  -- *  \sa SDL_GetVideoDriver()
  --  

   function SDL_GetNumVideoDrivers return int;  -- ..\SDL2_tmp\SDL_video.h:263
   pragma Import (C, SDL_GetNumVideoDrivers, "SDL_GetNumVideoDrivers");

  --*
  -- *  \brief Get the name of a built in video driver.
  -- *
  -- *  \note The video drivers are presented in the order in which they are
  -- *        normally checked during initialization.
  -- *
  -- *  \sa SDL_GetNumVideoDrivers()
  --  

   function SDL_GetVideoDriver (index : int) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_video.h:273
   pragma Import (C, SDL_GetVideoDriver, "SDL_GetVideoDriver");

  --*
  -- *  \brief Initialize the video subsystem, optionally specifying a video driver.
  -- *
  -- *  \param driver_name Initialize a specific driver by name, or NULL for the
  -- *                     default video driver.
  -- *
  -- *  \return 0 on success, -1 on error
  -- *
  -- *  This function initializes the video subsystem; setting up a connection
  -- *  to the window manager, etc, and determines the available display modes
  -- *  and pixel formats, but does not initialize a window or graphics mode.
  -- *
  -- *  \sa SDL_VideoQuit()
  --  

   function SDL_VideoInit (driver_name : Interfaces.C.Strings.chars_ptr) return int;  -- ..\SDL2_tmp\SDL_video.h:289
   pragma Import (C, SDL_VideoInit, "SDL_VideoInit");

  --*
  -- *  \brief Shuts down the video subsystem.
  -- *
  -- *  This function closes all windows, and restores the original video mode.
  -- *
  -- *  \sa SDL_VideoInit()
  --  

   procedure SDL_VideoQuit;  -- ..\SDL2_tmp\SDL_video.h:298
   pragma Import (C, SDL_VideoQuit, "SDL_VideoQuit");

  --*
  -- *  \brief Returns the name of the currently initialized video driver.
  -- *
  -- *  \return The name of the current video driver or NULL if no driver
  -- *          has been initialized
  -- *
  -- *  \sa SDL_GetNumVideoDrivers()
  -- *  \sa SDL_GetVideoDriver()
  --  

   function SDL_GetCurrentVideoDriver return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_video.h:309
   pragma Import (C, SDL_GetCurrentVideoDriver, "SDL_GetCurrentVideoDriver");

  --*
  -- *  \brief Returns the number of available video displays.
  -- *
  -- *  \sa SDL_GetDisplayBounds()
  --  

   function SDL_GetNumVideoDisplays return int;  -- ..\SDL2_tmp\SDL_video.h:316
   pragma Import (C, SDL_GetNumVideoDisplays, "SDL_GetNumVideoDisplays");

  --*
  -- *  \brief Get the name of a display in UTF-8 encoding
  -- *
  -- *  \return The name of a display, or NULL for an invalid display index.
  -- *
  -- *  \sa SDL_GetNumVideoDisplays()
  --  

   function SDL_GetDisplayName (displayIndex : int) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_video.h:325
   pragma Import (C, SDL_GetDisplayName, "SDL_GetDisplayName");

  --*
  -- *  \brief Get the desktop area represented by a display, with the primary
  -- *         display located at 0,0
  -- *
  -- *  \return 0 on success, or -1 if the index is out of range.
  -- *
  -- *  \sa SDL_GetNumVideoDisplays()
  --  

   function SDL_GetDisplayBounds (displayIndex : int; rect : access SDL_rect_h.SDL_Rect) return int;  -- ..\SDL2_tmp\SDL_video.h:335
   pragma Import (C, SDL_GetDisplayBounds, "SDL_GetDisplayBounds");

  --*
  -- *  \brief Get the usable desktop area represented by a display, with the
  -- *         primary display located at 0,0
  -- *
  -- *  This is the same area as SDL_GetDisplayBounds() reports, but with portions
  -- *  reserved by the system removed. For example, on Mac OS X, this subtracts
  -- *  the area occupied by the menu bar and dock.
  -- *
  -- *  Setting a window to be fullscreen generally bypasses these unusable areas,
  -- *  so these are good guidelines for the maximum space available to a
  -- *  non-fullscreen window.
  -- *
  -- *  \return 0 on success, or -1 if the index is out of range.
  -- *
  -- *  \sa SDL_GetDisplayBounds()
  -- *  \sa SDL_GetNumVideoDisplays()
  --  

   function SDL_GetDisplayUsableBounds (displayIndex : int; rect : access SDL_rect_h.SDL_Rect) return int;  -- ..\SDL2_tmp\SDL_video.h:354
   pragma Import (C, SDL_GetDisplayUsableBounds, "SDL_GetDisplayUsableBounds");

  --*
  -- *  \brief Get the dots/pixels-per-inch for a display
  -- *
  -- *  \note Diagonal, horizontal and vertical DPI can all be optionally
  -- *        returned if the parameter is non-NULL.
  -- *
  -- *  \return 0 on success, or -1 if no DPI information is available or the index is out of range.
  -- *
  -- *  \sa SDL_GetNumVideoDisplays()
  --  

   function SDL_GetDisplayDPI
     (displayIndex : int;
      ddpi : access float;
      hdpi : access float;
      vdpi : access float) return int;  -- ..\SDL2_tmp\SDL_video.h:366
   pragma Import (C, SDL_GetDisplayDPI, "SDL_GetDisplayDPI");

  --*
  -- *  \brief Get the orientation of a display
  -- *
  -- *  \return The orientation of the display, or SDL_ORIENTATION_UNKNOWN if it isn't available.
  -- *
  -- *  \sa SDL_GetNumVideoDisplays()
  --  

   function SDL_GetDisplayOrientation (displayIndex : int) return SDL_DisplayOrientation;  -- ..\SDL2_tmp\SDL_video.h:375
   pragma Import (C, SDL_GetDisplayOrientation, "SDL_GetDisplayOrientation");

  --*
  -- *  \brief Returns the number of available display modes.
  -- *
  -- *  \sa SDL_GetDisplayMode()
  --  

   function SDL_GetNumDisplayModes (displayIndex : int) return int;  -- ..\SDL2_tmp\SDL_video.h:382
   pragma Import (C, SDL_GetNumDisplayModes, "SDL_GetNumDisplayModes");

  --*
  -- *  \brief Fill in information about a specific display mode.
  -- *
  -- *  \note The display modes are sorted in this priority:
  -- *        \li bits per pixel -> more colors to fewer colors
  -- *        \li width -> largest to smallest
  -- *        \li height -> largest to smallest
  -- *        \li refresh rate -> highest to lowest
  -- *
  -- *  \sa SDL_GetNumDisplayModes()
  --  

   function SDL_GetDisplayMode
     (displayIndex : int;
      modeIndex : int;
      mode : access SDL_DisplayMode) return int;  -- ..\SDL2_tmp\SDL_video.h:395
   pragma Import (C, SDL_GetDisplayMode, "SDL_GetDisplayMode");

  --*
  -- *  \brief Fill in information about the desktop display mode.
  --  

   function SDL_GetDesktopDisplayMode (displayIndex : int; mode : access SDL_DisplayMode) return int;  -- ..\SDL2_tmp\SDL_video.h:401
   pragma Import (C, SDL_GetDesktopDisplayMode, "SDL_GetDesktopDisplayMode");

  --*
  -- *  \brief Fill in information about the current display mode.
  --  

   function SDL_GetCurrentDisplayMode (displayIndex : int; mode : access SDL_DisplayMode) return int;  -- ..\SDL2_tmp\SDL_video.h:406
   pragma Import (C, SDL_GetCurrentDisplayMode, "SDL_GetCurrentDisplayMode");

  --*
  -- *  \brief Get the closest match to the requested display mode.
  -- *
  -- *  \param displayIndex The index of display from which mode should be queried.
  -- *  \param mode The desired display mode
  -- *  \param closest A pointer to a display mode to be filled in with the closest
  -- *                 match of the available display modes.
  -- *
  -- *  \return The passed in value \c closest, or NULL if no matching video mode
  -- *          was available.
  -- *
  -- *  The available display modes are scanned, and \c closest is filled in with the
  -- *  closest mode matching the requested mode and returned.  The mode format and
  -- *  refresh_rate default to the desktop mode if they are 0.  The modes are
  -- *  scanned with size being first priority, format being second priority, and
  -- *  finally checking the refresh_rate.  If all the available modes are too
  -- *  small, then NULL is returned.
  -- *
  -- *  \sa SDL_GetNumDisplayModes()
  -- *  \sa SDL_GetDisplayMode()
  --  

   function SDL_GetClosestDisplayMode
     (displayIndex : int;
      mode : access constant SDL_DisplayMode;
      closest : access SDL_DisplayMode) return access SDL_DisplayMode;  -- ..\SDL2_tmp\SDL_video.h:430
   pragma Import (C, SDL_GetClosestDisplayMode, "SDL_GetClosestDisplayMode");

  --*
  -- *  \brief Get the display index associated with a window.
  -- *
  -- *  \return the display index of the display containing the center of the
  -- *          window, or -1 on error.
  --  

   function SDL_GetWindowDisplayIndex (window : access SDL_Window) return int;  -- ..\SDL2_tmp\SDL_video.h:438
   pragma Import (C, SDL_GetWindowDisplayIndex, "SDL_GetWindowDisplayIndex");

  --*
  -- *  \brief Set the display mode used when a fullscreen window is visible.
  -- *
  -- *  By default the window's dimensions and the desktop format and refresh rate
  -- *  are used.
  -- *
  -- *  \param window The window for which the display mode should be set.
  -- *  \param mode The mode to use, or NULL for the default mode.
  -- *
  -- *  \return 0 on success, or -1 if setting the display mode failed.
  -- *
  -- *  \sa SDL_GetWindowDisplayMode()
  -- *  \sa SDL_SetWindowFullscreen()
  --  

   function SDL_SetWindowDisplayMode (window : access SDL_Window; mode : access constant SDL_DisplayMode) return int;  -- ..\SDL2_tmp\SDL_video.h:454
   pragma Import (C, SDL_SetWindowDisplayMode, "SDL_SetWindowDisplayMode");

  --*
  -- *  \brief Fill in information about the display mode used when a fullscreen
  -- *         window is visible.
  -- *
  -- *  \sa SDL_SetWindowDisplayMode()
  -- *  \sa SDL_SetWindowFullscreen()
  --  

   function SDL_GetWindowDisplayMode (window : access SDL_Window; mode : access SDL_DisplayMode) return int;  -- ..\SDL2_tmp\SDL_video.h:465
   pragma Import (C, SDL_GetWindowDisplayMode, "SDL_GetWindowDisplayMode");

  --*
  -- *  \brief Get the pixel format associated with the window.
  --  

   function SDL_GetWindowPixelFormat (window : access SDL_Window) return SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_video.h:471
   pragma Import (C, SDL_GetWindowPixelFormat, "SDL_GetWindowPixelFormat");

  --*
  -- *  \brief Create a window with the specified position, dimensions, and flags.
  -- *
  -- *  \param title The title of the window, in UTF-8 encoding.
  -- *  \param x     The x position of the window, ::SDL_WINDOWPOS_CENTERED, or
  -- *               ::SDL_WINDOWPOS_UNDEFINED.
  -- *  \param y     The y position of the window, ::SDL_WINDOWPOS_CENTERED, or
  -- *               ::SDL_WINDOWPOS_UNDEFINED.
  -- *  \param w     The width of the window, in screen coordinates.
  -- *  \param h     The height of the window, in screen coordinates.
  -- *  \param flags The flags for the window, a mask of any of the following:
  -- *               ::SDL_WINDOW_FULLSCREEN,    ::SDL_WINDOW_OPENGL,
  -- *               ::SDL_WINDOW_HIDDEN,        ::SDL_WINDOW_BORDERLESS,
  -- *               ::SDL_WINDOW_RESIZABLE,     ::SDL_WINDOW_MAXIMIZED,
  -- *               ::SDL_WINDOW_MINIMIZED,     ::SDL_WINDOW_INPUT_GRABBED,
  -- *               ::SDL_WINDOW_ALLOW_HIGHDPI, ::SDL_WINDOW_VULKAN.
  -- *
  -- *  \return The created window, or NULL if window creation failed.
  -- *
  -- *  If the window is created with the SDL_WINDOW_ALLOW_HIGHDPI flag, its size
  -- *  in pixels may differ from its size in screen coordinates on platforms with
  -- *  high-DPI support (e.g. iOS and Mac OS X). Use SDL_GetWindowSize() to query
  -- *  the client area's size in screen coordinates, and SDL_GL_GetDrawableSize(),
  -- *  SDL_Vulkan_GetDrawableSize(), or SDL_GetRendererOutputSize() to query the
  -- *  drawable size in pixels.
  -- *
  -- *  If the window is created with any of the SDL_WINDOW_OPENGL or
  -- *  SDL_WINDOW_VULKAN flags, then the corresponding LoadLibrary function
  -- *  (SDL_GL_LoadLibrary or SDL_Vulkan_LoadLibrary) is called and the
  -- *  corresponding UnloadLibrary function is called by SDL_DestroyWindow().
  -- *
  -- *  If SDL_WINDOW_VULKAN is specified and there isn't a working Vulkan driver,
  -- *  SDL_CreateWindow() will fail because SDL_Vulkan_LoadLibrary() will fail.
  -- *
  -- *  \note On non-Apple devices, SDL requires you to either not link to the
  -- *        Vulkan loader or link to a dynamic library version. This limitation
  -- *        may be removed in a future version of SDL.
  -- *
  -- *  \sa SDL_DestroyWindow()
  -- *  \sa SDL_GL_LoadLibrary()
  -- *  \sa SDL_Vulkan_LoadLibrary()
  --  

   function SDL_CreateWindow
     (title : Interfaces.C.Strings.chars_ptr;
      x : int;
      y : int;
      w : int;
      h : int;
      flags : SDL_stdinc_h.Uint32) return access SDL_Window;  -- ..\SDL2_tmp\SDL_video.h:515
   pragma Import (C, SDL_CreateWindow, "SDL_CreateWindow");

  --*
  -- *  \brief Create an SDL window from an existing native window.
  -- *
  -- *  \param data A pointer to driver-dependent window creation data
  -- *
  -- *  \return The created window, or NULL if window creation failed.
  -- *
  -- *  \sa SDL_DestroyWindow()
  --  

   function SDL_CreateWindowFrom (data : System.Address) return access SDL_Window;  -- ..\SDL2_tmp\SDL_video.h:528
   pragma Import (C, SDL_CreateWindowFrom, "SDL_CreateWindowFrom");

  --*
  -- *  \brief Get the numeric ID of a window, for logging purposes.
  --  

   function SDL_GetWindowID (window : access SDL_Window) return SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_video.h:533
   pragma Import (C, SDL_GetWindowID, "SDL_GetWindowID");

  --*
  -- *  \brief Get a window from a stored ID, or NULL if it doesn't exist.
  --  

   function SDL_GetWindowFromID (id : SDL_stdinc_h.Uint32) return access SDL_Window;  -- ..\SDL2_tmp\SDL_video.h:538
   pragma Import (C, SDL_GetWindowFromID, "SDL_GetWindowFromID");

  --*
  -- *  \brief Get the window flags.
  --  

   function SDL_GetWindowFlags (window : access SDL_Window) return SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_video.h:543
   pragma Import (C, SDL_GetWindowFlags, "SDL_GetWindowFlags");

  --*
  -- *  \brief Set the title of a window, in UTF-8 format.
  -- *
  -- *  \sa SDL_GetWindowTitle()
  --  

   procedure SDL_SetWindowTitle (window : access SDL_Window; title : Interfaces.C.Strings.chars_ptr);  -- ..\SDL2_tmp\SDL_video.h:550
   pragma Import (C, SDL_SetWindowTitle, "SDL_SetWindowTitle");

  --*
  -- *  \brief Get the title of a window, in UTF-8 format.
  -- *
  -- *  \sa SDL_SetWindowTitle()
  --  

   function SDL_GetWindowTitle (window : access SDL_Window) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_video.h:558
   pragma Import (C, SDL_GetWindowTitle, "SDL_GetWindowTitle");

  --*
  -- *  \brief Set the icon for a window.
  -- *
  -- *  \param window The window for which the icon should be set.
  -- *  \param icon The icon for the window.
  --  

   procedure SDL_SetWindowIcon (window : access SDL_Window; icon : access SDL_surface_h.SDL_Surface);  -- ..\SDL2_tmp\SDL_video.h:566
   pragma Import (C, SDL_SetWindowIcon, "SDL_SetWindowIcon");

  --*
  -- *  \brief Associate an arbitrary named pointer with a window.
  -- *
  -- *  \param window   The window to associate with the pointer.
  -- *  \param name     The name of the pointer.
  -- *  \param userdata The associated pointer.
  -- *
  -- *  \return The previous value associated with 'name'
  -- *
  -- *  \note The name is case-sensitive.
  -- *
  -- *  \sa SDL_GetWindowData()
  --  

   function SDL_SetWindowData
     (window : access SDL_Window;
      name : Interfaces.C.Strings.chars_ptr;
      userdata : System.Address) return System.Address;  -- ..\SDL2_tmp\SDL_video.h:582
   pragma Import (C, SDL_SetWindowData, "SDL_SetWindowData");

  --*
  -- *  \brief Retrieve the data pointer associated with a window.
  -- *
  -- *  \param window   The window to query.
  -- *  \param name     The name of the pointer.
  -- *
  -- *  \return The value associated with 'name'
  -- *
  -- *  \sa SDL_SetWindowData()
  --  

   function SDL_GetWindowData (window : access SDL_Window; name : Interfaces.C.Strings.chars_ptr) return System.Address;  -- ..\SDL2_tmp\SDL_video.h:596
   pragma Import (C, SDL_GetWindowData, "SDL_GetWindowData");

  --*
  -- *  \brief Set the position of a window.
  -- *
  -- *  \param window   The window to reposition.
  -- *  \param x        The x coordinate of the window in screen coordinates, or
  -- *                  ::SDL_WINDOWPOS_CENTERED or ::SDL_WINDOWPOS_UNDEFINED.
  -- *  \param y        The y coordinate of the window in screen coordinates, or
  -- *                  ::SDL_WINDOWPOS_CENTERED or ::SDL_WINDOWPOS_UNDEFINED.
  -- *
  -- *  \note The window coordinate origin is the upper left of the display.
  -- *
  -- *  \sa SDL_GetWindowPosition()
  --  

   procedure SDL_SetWindowPosition
     (window : access SDL_Window;
      x : int;
      y : int);  -- ..\SDL2_tmp\SDL_video.h:612
   pragma Import (C, SDL_SetWindowPosition, "SDL_SetWindowPosition");

  --*
  -- *  \brief Get the position of a window.
  -- *
  -- *  \param window   The window to query.
  -- *  \param x        Pointer to variable for storing the x position, in screen
  -- *                  coordinates. May be NULL.
  -- *  \param y        Pointer to variable for storing the y position, in screen
  -- *                  coordinates. May be NULL.
  -- *
  -- *  \sa SDL_SetWindowPosition()
  --  

   procedure SDL_GetWindowPosition
     (window : access SDL_Window;
      x : access int;
      y : access int);  -- ..\SDL2_tmp\SDL_video.h:626
   pragma Import (C, SDL_GetWindowPosition, "SDL_GetWindowPosition");

  --*
  -- *  \brief Set the size of a window's client area.
  -- *
  -- *  \param window   The window to resize.
  -- *  \param w        The width of the window, in screen coordinates. Must be >0.
  -- *  \param h        The height of the window, in screen coordinates. Must be >0.
  -- *
  -- *  \note Fullscreen windows automatically match the size of the display mode,
  -- *        and you should use SDL_SetWindowDisplayMode() to change their size.
  -- *
  -- *  The window size in screen coordinates may differ from the size in pixels, if
  -- *  the window was created with SDL_WINDOW_ALLOW_HIGHDPI on a platform with
  -- *  high-dpi support (e.g. iOS or OS X). Use SDL_GL_GetDrawableSize() or
  -- *  SDL_GetRendererOutputSize() to get the real client area size in pixels.
  -- *
  -- *  \sa SDL_GetWindowSize()
  -- *  \sa SDL_SetWindowDisplayMode()
  --  

   procedure SDL_SetWindowSize
     (window : access SDL_Window;
      w : int;
      h : int);  -- ..\SDL2_tmp\SDL_video.h:647
   pragma Import (C, SDL_SetWindowSize, "SDL_SetWindowSize");

  --*
  -- *  \brief Get the size of a window's client area.
  -- *
  -- *  \param window   The window to query.
  -- *  \param w        Pointer to variable for storing the width, in screen
  -- *                  coordinates. May be NULL.
  -- *  \param h        Pointer to variable for storing the height, in screen
  -- *                  coordinates. May be NULL.
  -- *
  -- *  The window size in screen coordinates may differ from the size in pixels, if
  -- *  the window was created with SDL_WINDOW_ALLOW_HIGHDPI on a platform with
  -- *  high-dpi support (e.g. iOS or OS X). Use SDL_GL_GetDrawableSize() or
  -- *  SDL_GetRendererOutputSize() to get the real client area size in pixels.
  -- *
  -- *  \sa SDL_SetWindowSize()
  --  

   procedure SDL_GetWindowSize
     (window : access SDL_Window;
      w : access int;
      h : access int);  -- ..\SDL2_tmp\SDL_video.h:666
   pragma Import (C, SDL_GetWindowSize, "SDL_GetWindowSize");

  --*
  -- *  \brief Get the size of a window's borders (decorations) around the client area.
  -- *
  -- *  \param window The window to query.
  -- *  \param top Pointer to variable for storing the size of the top border. NULL is permitted.
  -- *  \param left Pointer to variable for storing the size of the left border. NULL is permitted.
  -- *  \param bottom Pointer to variable for storing the size of the bottom border. NULL is permitted.
  -- *  \param right Pointer to variable for storing the size of the right border. NULL is permitted.
  -- *
  -- *  \return 0 on success, or -1 if getting this information is not supported.
  -- *
  -- *  \note if this function fails (returns -1), the size values will be
  -- *        initialized to 0, 0, 0, 0 (if a non-NULL pointer is provided), as
  -- *        if the window in question was borderless.
  --  

   function SDL_GetWindowBordersSize
     (window : access SDL_Window;
      top : access int;
      left : access int;
      bottom : access int;
      right : access int) return int;  -- ..\SDL2_tmp\SDL_video.h:684
   pragma Import (C, SDL_GetWindowBordersSize, "SDL_GetWindowBordersSize");

  --*
  -- *  \brief Set the minimum size of a window's client area.
  -- *
  -- *  \param window    The window to set a new minimum size.
  -- *  \param min_w     The minimum width of the window, must be >0
  -- *  \param min_h     The minimum height of the window, must be >0
  -- *
  -- *  \note You can't change the minimum size of a fullscreen window, it
  -- *        automatically matches the size of the display mode.
  -- *
  -- *  \sa SDL_GetWindowMinimumSize()
  -- *  \sa SDL_SetWindowMaximumSize()
  --  

   procedure SDL_SetWindowMinimumSize
     (window : access SDL_Window;
      min_w : int;
      min_h : int);  -- ..\SDL2_tmp\SDL_video.h:701
   pragma Import (C, SDL_SetWindowMinimumSize, "SDL_SetWindowMinimumSize");

  --*
  -- *  \brief Get the minimum size of a window's client area.
  -- *
  -- *  \param window   The window to query.
  -- *  \param w        Pointer to variable for storing the minimum width, may be NULL
  -- *  \param h        Pointer to variable for storing the minimum height, may be NULL
  -- *
  -- *  \sa SDL_GetWindowMaximumSize()
  -- *  \sa SDL_SetWindowMinimumSize()
  --  

   procedure SDL_GetWindowMinimumSize
     (window : access SDL_Window;
      w : access int;
      h : access int);  -- ..\SDL2_tmp\SDL_video.h:714
   pragma Import (C, SDL_GetWindowMinimumSize, "SDL_GetWindowMinimumSize");

  --*
  -- *  \brief Set the maximum size of a window's client area.
  -- *
  -- *  \param window    The window to set a new maximum size.
  -- *  \param max_w     The maximum width of the window, must be >0
  -- *  \param max_h     The maximum height of the window, must be >0
  -- *
  -- *  \note You can't change the maximum size of a fullscreen window, it
  -- *        automatically matches the size of the display mode.
  -- *
  -- *  \sa SDL_GetWindowMaximumSize()
  -- *  \sa SDL_SetWindowMinimumSize()
  --  

   procedure SDL_SetWindowMaximumSize
     (window : access SDL_Window;
      max_w : int;
      max_h : int);  -- ..\SDL2_tmp\SDL_video.h:730
   pragma Import (C, SDL_SetWindowMaximumSize, "SDL_SetWindowMaximumSize");

  --*
  -- *  \brief Get the maximum size of a window's client area.
  -- *
  -- *  \param window   The window to query.
  -- *  \param w        Pointer to variable for storing the maximum width, may be NULL
  -- *  \param h        Pointer to variable for storing the maximum height, may be NULL
  -- *
  -- *  \sa SDL_GetWindowMinimumSize()
  -- *  \sa SDL_SetWindowMaximumSize()
  --  

   procedure SDL_GetWindowMaximumSize
     (window : access SDL_Window;
      w : access int;
      h : access int);  -- ..\SDL2_tmp\SDL_video.h:743
   pragma Import (C, SDL_GetWindowMaximumSize, "SDL_GetWindowMaximumSize");

  --*
  -- *  \brief Set the border state of a window.
  -- *
  -- *  This will add or remove the window's SDL_WINDOW_BORDERLESS flag and
  -- *  add or remove the border from the actual window. This is a no-op if the
  -- *  window's border already matches the requested state.
  -- *
  -- *  \param window The window of which to change the border state.
  -- *  \param bordered SDL_FALSE to remove border, SDL_TRUE to add border.
  -- *
  -- *  \note You can't change the border state of a fullscreen window.
  -- *
  -- *  \sa SDL_GetWindowFlags()
  --  

   procedure SDL_SetWindowBordered (window : access SDL_Window; bordered : SDL_stdinc_h.SDL_bool);  -- ..\SDL2_tmp\SDL_video.h:760
   pragma Import (C, SDL_SetWindowBordered, "SDL_SetWindowBordered");

  --*
  -- *  \brief Set the user-resizable state of a window.
  -- *
  -- *  This will add or remove the window's SDL_WINDOW_RESIZABLE flag and
  -- *  allow/disallow user resizing of the window. This is a no-op if the
  -- *  window's resizable state already matches the requested state.
  -- *
  -- *  \param window The window of which to change the resizable state.
  -- *  \param resizable SDL_TRUE to allow resizing, SDL_FALSE to disallow.
  -- *
  -- *  \note You can't change the resizable state of a fullscreen window.
  -- *
  -- *  \sa SDL_GetWindowFlags()
  --  

   procedure SDL_SetWindowResizable (window : access SDL_Window; resizable : SDL_stdinc_h.SDL_bool);  -- ..\SDL2_tmp\SDL_video.h:777
   pragma Import (C, SDL_SetWindowResizable, "SDL_SetWindowResizable");

  --*
  -- *  \brief Show a window.
  -- *
  -- *  \sa SDL_HideWindow()
  --  

   procedure SDL_ShowWindow (window : access SDL_Window);  -- ..\SDL2_tmp\SDL_video.h:785
   pragma Import (C, SDL_ShowWindow, "SDL_ShowWindow");

  --*
  -- *  \brief Hide a window.
  -- *
  -- *  \sa SDL_ShowWindow()
  --  

   procedure SDL_HideWindow (window : access SDL_Window);  -- ..\SDL2_tmp\SDL_video.h:792
   pragma Import (C, SDL_HideWindow, "SDL_HideWindow");

  --*
  -- *  \brief Raise a window above other windows and set the input focus.
  --  

   procedure SDL_RaiseWindow (window : access SDL_Window);  -- ..\SDL2_tmp\SDL_video.h:797
   pragma Import (C, SDL_RaiseWindow, "SDL_RaiseWindow");

  --*
  -- *  \brief Make a window as large as possible.
  -- *
  -- *  \sa SDL_RestoreWindow()
  --  

   procedure SDL_MaximizeWindow (window : access SDL_Window);  -- ..\SDL2_tmp\SDL_video.h:804
   pragma Import (C, SDL_MaximizeWindow, "SDL_MaximizeWindow");

  --*
  -- *  \brief Minimize a window to an iconic representation.
  -- *
  -- *  \sa SDL_RestoreWindow()
  --  

   procedure SDL_MinimizeWindow (window : access SDL_Window);  -- ..\SDL2_tmp\SDL_video.h:811
   pragma Import (C, SDL_MinimizeWindow, "SDL_MinimizeWindow");

  --*
  -- *  \brief Restore the size and position of a minimized or maximized window.
  -- *
  -- *  \sa SDL_MaximizeWindow()
  -- *  \sa SDL_MinimizeWindow()
  --  

   procedure SDL_RestoreWindow (window : access SDL_Window);  -- ..\SDL2_tmp\SDL_video.h:819
   pragma Import (C, SDL_RestoreWindow, "SDL_RestoreWindow");

  --*
  -- *  \brief Set a window's fullscreen state.
  -- *
  -- *  \return 0 on success, or -1 if setting the display mode failed.
  -- *
  -- *  \sa SDL_SetWindowDisplayMode()
  -- *  \sa SDL_GetWindowDisplayMode()
  --  

   function SDL_SetWindowFullscreen (window : access SDL_Window; flags : SDL_stdinc_h.Uint32) return int;  -- ..\SDL2_tmp\SDL_video.h:829
   pragma Import (C, SDL_SetWindowFullscreen, "SDL_SetWindowFullscreen");

  --*
  -- *  \brief Get the SDL surface associated with the window.
  -- *
  -- *  \return The window's framebuffer surface, or NULL on error.
  -- *
  -- *  A new surface will be created with the optimal format for the window,
  -- *  if necessary. This surface will be freed when the window is destroyed.
  -- *
  -- *  \note You may not combine this with 3D or the rendering API on this window.
  -- *
  -- *  \sa SDL_UpdateWindowSurface()
  -- *  \sa SDL_UpdateWindowSurfaceRects()
  --  

   function SDL_GetWindowSurface (window : access SDL_Window) return access SDL_surface_h.SDL_Surface;  -- ..\SDL2_tmp\SDL_video.h:845
   pragma Import (C, SDL_GetWindowSurface, "SDL_GetWindowSurface");

  --*
  -- *  \brief Copy the window surface to the screen.
  -- *
  -- *  \return 0 on success, or -1 on error.
  -- *
  -- *  \sa SDL_GetWindowSurface()
  -- *  \sa SDL_UpdateWindowSurfaceRects()
  --  

   function SDL_UpdateWindowSurface (window : access SDL_Window) return int;  -- ..\SDL2_tmp\SDL_video.h:855
   pragma Import (C, SDL_UpdateWindowSurface, "SDL_UpdateWindowSurface");

  --*
  -- *  \brief Copy a number of rectangles on the window surface to the screen.
  -- *
  -- *  \return 0 on success, or -1 on error.
  -- *
  -- *  \sa SDL_GetWindowSurface()
  -- *  \sa SDL_UpdateWindowSurface()
  --  

   function SDL_UpdateWindowSurfaceRects
     (window : access SDL_Window;
      rects : access constant SDL_rect_h.SDL_Rect;
      numrects : int) return int;  -- ..\SDL2_tmp\SDL_video.h:865
   pragma Import (C, SDL_UpdateWindowSurfaceRects, "SDL_UpdateWindowSurfaceRects");

  --*
  -- *  \brief Set a window's input grab mode.
  -- *
  -- *  \param window The window for which the input grab mode should be set.
  -- *  \param grabbed This is SDL_TRUE to grab input, and SDL_FALSE to release input.
  -- *
  -- *  If the caller enables a grab while another window is currently grabbed,
  -- *  the other window loses its grab in favor of the caller's window.
  -- *
  -- *  \sa SDL_GetWindowGrab()
  --  

   procedure SDL_SetWindowGrab (window : access SDL_Window; grabbed : SDL_stdinc_h.SDL_bool);  -- ..\SDL2_tmp\SDL_video.h:880
   pragma Import (C, SDL_SetWindowGrab, "SDL_SetWindowGrab");

  --*
  -- *  \brief Get a window's input grab mode.
  -- *
  -- *  \return This returns SDL_TRUE if input is grabbed, and SDL_FALSE otherwise.
  -- *
  -- *  \sa SDL_SetWindowGrab()
  --  

   function SDL_GetWindowGrab (window : access SDL_Window) return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_video.h:890
   pragma Import (C, SDL_GetWindowGrab, "SDL_GetWindowGrab");

  --*
  -- *  \brief Get the window that currently has an input grab enabled.
  -- *
  -- *  \return This returns the window if input is grabbed, and NULL otherwise.
  -- *
  -- *  \sa SDL_SetWindowGrab()
  --  

   function SDL_GetGrabbedWindow return access SDL_Window;  -- ..\SDL2_tmp\SDL_video.h:899
   pragma Import (C, SDL_GetGrabbedWindow, "SDL_GetGrabbedWindow");

  --*
  -- *  \brief Set the brightness (gamma correction) for a window.
  -- *
  -- *  \return 0 on success, or -1 if setting the brightness isn't supported.
  -- *
  -- *  \sa SDL_GetWindowBrightness()
  -- *  \sa SDL_SetWindowGammaRamp()
  --  

   function SDL_SetWindowBrightness (window : access SDL_Window; brightness : float) return int;  -- ..\SDL2_tmp\SDL_video.h:909
   pragma Import (C, SDL_SetWindowBrightness, "SDL_SetWindowBrightness");

  --*
  -- *  \brief Get the brightness (gamma correction) for a window.
  -- *
  -- *  \return The last brightness value passed to SDL_SetWindowBrightness()
  -- *
  -- *  \sa SDL_SetWindowBrightness()
  --  

   function SDL_GetWindowBrightness (window : access SDL_Window) return float;  -- ..\SDL2_tmp\SDL_video.h:918
   pragma Import (C, SDL_GetWindowBrightness, "SDL_GetWindowBrightness");

  --*
  -- *  \brief Set the opacity for a window
  -- *
  -- *  \param window The window which will be made transparent or opaque
  -- *  \param opacity Opacity (0.0f - transparent, 1.0f - opaque) This will be
  -- *                 clamped internally between 0.0f and 1.0f.
  -- *
  -- *  \return 0 on success, or -1 if setting the opacity isn't supported.
  -- *
  -- *  \sa SDL_GetWindowOpacity()
  --  

   function SDL_SetWindowOpacity (window : access SDL_Window; opacity : float) return int;  -- ..\SDL2_tmp\SDL_video.h:931
   pragma Import (C, SDL_SetWindowOpacity, "SDL_SetWindowOpacity");

  --*
  -- *  \brief Get the opacity of a window.
  -- *
  -- *  If transparency isn't supported on this platform, opacity will be reported
  -- *  as 1.0f without error.
  -- *
  -- *  \param window The window in question.
  -- *  \param out_opacity Opacity (0.0f - transparent, 1.0f - opaque)
  -- *
  -- *  \return 0 on success, or -1 on error (invalid window, etc).
  -- *
  -- *  \sa SDL_SetWindowOpacity()
  --  

   function SDL_GetWindowOpacity (window : access SDL_Window; out_opacity : access float) return int;  -- ..\SDL2_tmp\SDL_video.h:946
   pragma Import (C, SDL_GetWindowOpacity, "SDL_GetWindowOpacity");

  --*
  -- *  \brief Sets the window as a modal for another window (TODO: reconsider this function and/or its name)
  -- *
  -- *  \param modal_window The window that should be modal
  -- *  \param parent_window The parent window
  -- *
  -- *  \return 0 on success, or -1 otherwise.
  --  

   function SDL_SetWindowModalFor (modal_window : access SDL_Window; parent_window : access SDL_Window) return int;  -- ..\SDL2_tmp\SDL_video.h:956
   pragma Import (C, SDL_SetWindowModalFor, "SDL_SetWindowModalFor");

  --*
  -- *  \brief Explicitly sets input focus to the window.
  -- *
  -- *  You almost certainly want SDL_RaiseWindow() instead of this function. Use
  -- *  this with caution, as you might give focus to a window that's completely
  -- *  obscured by other windows.
  -- *
  -- *  \param window The window that should get the input focus
  -- *
  -- *  \return 0 on success, or -1 otherwise.
  -- *  \sa SDL_RaiseWindow()
  --  

   function SDL_SetWindowInputFocus (window : access SDL_Window) return int;  -- ..\SDL2_tmp\SDL_video.h:970
   pragma Import (C, SDL_SetWindowInputFocus, "SDL_SetWindowInputFocus");

  --*
  -- *  \brief Set the gamma ramp for a window.
  -- *
  -- *  \param window The window for which the gamma ramp should be set.
  -- *  \param red The translation table for the red channel, or NULL.
  -- *  \param green The translation table for the green channel, or NULL.
  -- *  \param blue The translation table for the blue channel, or NULL.
  -- *
  -- *  \return 0 on success, or -1 if gamma ramps are unsupported.
  -- *
  -- *  Set the gamma translation table for the red, green, and blue channels
  -- *  of the video hardware.  Each table is an array of 256 16-bit quantities,
  -- *  representing a mapping between the input and output for that channel.
  -- *  The input is the index into the array, and the output is the 16-bit
  -- *  gamma value at that index, scaled to the output color precision.
  -- *
  -- *  \sa SDL_GetWindowGammaRamp()
  --  

   function SDL_SetWindowGammaRamp
     (window : access SDL_Window;
      red : access SDL_stdinc_h.Uint16;
      green : access SDL_stdinc_h.Uint16;
      blue : access SDL_stdinc_h.Uint16) return int;  -- ..\SDL2_tmp\SDL_video.h:990
   pragma Import (C, SDL_SetWindowGammaRamp, "SDL_SetWindowGammaRamp");

  --*
  -- *  \brief Get the gamma ramp for a window.
  -- *
  -- *  \param window The window from which the gamma ramp should be queried.
  -- *  \param red   A pointer to a 256 element array of 16-bit quantities to hold
  -- *               the translation table for the red channel, or NULL.
  -- *  \param green A pointer to a 256 element array of 16-bit quantities to hold
  -- *               the translation table for the green channel, or NULL.
  -- *  \param blue  A pointer to a 256 element array of 16-bit quantities to hold
  -- *               the translation table for the blue channel, or NULL.
  -- *
  -- *  \return 0 on success, or -1 if gamma ramps are unsupported.
  -- *
  -- *  \sa SDL_SetWindowGammaRamp()
  --  

   function SDL_GetWindowGammaRamp
     (window : access SDL_Window;
      red : access SDL_stdinc_h.Uint16;
      green : access SDL_stdinc_h.Uint16;
      blue : access SDL_stdinc_h.Uint16) return int;  -- ..\SDL2_tmp\SDL_video.h:1010
   pragma Import (C, SDL_GetWindowGammaRamp, "SDL_GetWindowGammaRamp");

  --*
  -- *  \brief Possible return values from the SDL_HitTest callback.
  -- *
  -- *  \sa SDL_HitTest
  --  

  --*< Region is normal. No special properties.  
  --*< Region can drag entire window.  
   type SDL_HitTestResult is 
     (SDL_HITTEST_NORMAL,
      SDL_HITTEST_DRAGGABLE,
      SDL_HITTEST_RESIZE_TOPLEFT,
      SDL_HITTEST_RESIZE_TOP,
      SDL_HITTEST_RESIZE_TOPRIGHT,
      SDL_HITTEST_RESIZE_RIGHT,
      SDL_HITTEST_RESIZE_BOTTOMRIGHT,
      SDL_HITTEST_RESIZE_BOTTOM,
      SDL_HITTEST_RESIZE_BOTTOMLEFT,
      SDL_HITTEST_RESIZE_LEFT);
   pragma Convention (C, SDL_HitTestResult);  -- ..\SDL2_tmp\SDL_video.h:1032

  --*
  -- *  \brief Callback used for hit-testing.
  -- *
  -- *  \sa SDL_SetWindowHitTest
  --  

   type SDL_HitTest is access function
        (arg1 : access SDL_Window;
         arg2 : access constant SDL_rect_h.SDL_Point;
         arg3 : System.Address) return SDL_HitTestResult;
   pragma Convention (C, SDL_HitTest);  -- ..\SDL2_tmp\SDL_video.h:1039

  --*
  -- *  \brief Provide a callback that decides if a window region has special properties.
  -- *
  -- *  Normally windows are dragged and resized by decorations provided by the
  -- *  system window manager (a title bar, borders, etc), but for some apps, it
  -- *  makes sense to drag them from somewhere else inside the window itself; for
  -- *  example, one might have a borderless window that wants to be draggable
  -- *  from any part, or simulate its own title bar, etc.
  -- *
  -- *  This function lets the app provide a callback that designates pieces of
  -- *  a given window as special. This callback is run during event processing
  -- *  if we need to tell the OS to treat a region of the window specially; the
  -- *  use of this callback is known as "hit testing."
  -- *
  -- *  Mouse input may not be delivered to your application if it is within
  -- *  a special area; the OS will often apply that input to moving the window or
  -- *  resizing the window and not deliver it to the application.
  -- *
  -- *  Specifying NULL for a callback disables hit-testing. Hit-testing is
  -- *  disabled by default.
  -- *
  -- *  Platforms that don't support this functionality will return -1
  -- *  unconditionally, even if you're attempting to disable hit-testing.
  -- *
  -- *  Your callback may fire at any time, and its firing does not indicate any
  -- *  specific behavior (for example, on Windows, this certainly might fire
  -- *  when the OS is deciding whether to drag your window, but it fires for lots
  -- *  of other reasons, too, some unrelated to anything you probably care about
  -- *  _and when the mouse isn't actually at the location it is testing_).
  -- *  Since this can fire at any time, you should try to keep your callback
  -- *  efficient, devoid of allocations, etc.
  -- *
  -- *  \param window The window to set hit-testing on.
  -- *  \param callback The callback to call when doing a hit-test.
  -- *  \param callback_data An app-defined void pointer passed to the callback.
  -- *  \return 0 on success, -1 on error (including unsupported).
  --  

   function SDL_SetWindowHitTest
     (window : access SDL_Window;
      callback : SDL_HitTest;
      callback_data : System.Address) return int;  -- ..\SDL2_tmp\SDL_video.h:1080
   pragma Import (C, SDL_SetWindowHitTest, "SDL_SetWindowHitTest");

  --*
  -- *  \brief Destroy a window.
  --  

   procedure SDL_DestroyWindow (window : access SDL_Window);  -- ..\SDL2_tmp\SDL_video.h:1087
   pragma Import (C, SDL_DestroyWindow, "SDL_DestroyWindow");

  --*
  -- *  \brief Returns whether the screensaver is currently enabled (default off).
  -- *
  -- *  \sa SDL_EnableScreenSaver()
  -- *  \sa SDL_DisableScreenSaver()
  --  

   function SDL_IsScreenSaverEnabled return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_video.h:1096
   pragma Import (C, SDL_IsScreenSaverEnabled, "SDL_IsScreenSaverEnabled");

  --*
  -- *  \brief Allow the screen to be blanked by a screensaver
  -- *
  -- *  \sa SDL_IsScreenSaverEnabled()
  -- *  \sa SDL_DisableScreenSaver()
  --  

   procedure SDL_EnableScreenSaver;  -- ..\SDL2_tmp\SDL_video.h:1104
   pragma Import (C, SDL_EnableScreenSaver, "SDL_EnableScreenSaver");

  --*
  -- *  \brief Prevent the screen from being blanked by a screensaver
  -- *
  -- *  \sa SDL_IsScreenSaverEnabled()
  -- *  \sa SDL_EnableScreenSaver()
  --  

   procedure SDL_DisableScreenSaver;  -- ..\SDL2_tmp\SDL_video.h:1112
   pragma Import (C, SDL_DisableScreenSaver, "SDL_DisableScreenSaver");

  --*
  -- *  \name OpenGL support functions
  --  

  -- @{  
  --*
  -- *  \brief Dynamically load an OpenGL library.
  -- *
  -- *  \param path The platform dependent OpenGL library name, or NULL to open the
  -- *              default OpenGL library.
  -- *
  -- *  \return 0 on success, or -1 if the library couldn't be loaded.
  -- *
  -- *  This should be done after initializing the video driver, but before
  -- *  creating any OpenGL windows.  If no OpenGL library is loaded, the default
  -- *  library will be loaded upon creation of the first OpenGL window.
  -- *
  -- *  \note If you do this, you need to retrieve all of the GL functions used in
  -- *        your program from the dynamic library using SDL_GL_GetProcAddress().
  -- *
  -- *  \sa SDL_GL_GetProcAddress()
  -- *  \sa SDL_GL_UnloadLibrary()
  --  

   function SDL_GL_LoadLibrary (path : Interfaces.C.Strings.chars_ptr) return int;  -- ..\SDL2_tmp\SDL_video.h:1138
   pragma Import (C, SDL_GL_LoadLibrary, "SDL_GL_LoadLibrary");

  --*
  -- *  \brief Get the address of an OpenGL function.
  --  

   function SDL_GL_GetProcAddress (proc : Interfaces.C.Strings.chars_ptr) return System.Address;  -- ..\SDL2_tmp\SDL_video.h:1143
   pragma Import (C, SDL_GL_GetProcAddress, "SDL_GL_GetProcAddress");

  --*
  -- *  \brief Unload the OpenGL library previously loaded by SDL_GL_LoadLibrary().
  -- *
  -- *  \sa SDL_GL_LoadLibrary()
  --  

   procedure SDL_GL_UnloadLibrary;  -- ..\SDL2_tmp\SDL_video.h:1150
   pragma Import (C, SDL_GL_UnloadLibrary, "SDL_GL_UnloadLibrary");

  --*
  -- *  \brief Return true if an OpenGL extension is supported for the current
  -- *         context.
  --  

   function SDL_GL_ExtensionSupported (extension : Interfaces.C.Strings.chars_ptr) return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_video.h:1156
   pragma Import (C, SDL_GL_ExtensionSupported, "SDL_GL_ExtensionSupported");

  --*
  -- *  \brief Reset all previously set OpenGL context attributes to their default values
  --  

   procedure SDL_GL_ResetAttributes;  -- ..\SDL2_tmp\SDL_video.h:1162
   pragma Import (C, SDL_GL_ResetAttributes, "SDL_GL_ResetAttributes");

  --*
  -- *  \brief Set an OpenGL window attribute before window creation.
  -- *
  -- *  \return 0 on success, or -1 if the attribute could not be set.
  --  

   function SDL_GL_SetAttribute (attr : SDL_GLattr; value : int) return int;  -- ..\SDL2_tmp\SDL_video.h:1169
   pragma Import (C, SDL_GL_SetAttribute, "SDL_GL_SetAttribute");

  --*
  -- *  \brief Get the actual value for an attribute from the current context.
  -- *
  -- *  \return 0 on success, or -1 if the attribute could not be retrieved.
  -- *          The integer at \c value will be modified in either case.
  --  

   function SDL_GL_GetAttribute (attr : SDL_GLattr; value : access int) return int;  -- ..\SDL2_tmp\SDL_video.h:1177
   pragma Import (C, SDL_GL_GetAttribute, "SDL_GL_GetAttribute");

  --*
  -- *  \brief Create an OpenGL context for use with an OpenGL window, and make it
  -- *         current.
  -- *
  -- *  \sa SDL_GL_DeleteContext()
  --  

   function SDL_GL_CreateContext (window : access SDL_Window) return SDL_GLContext;  -- ..\SDL2_tmp\SDL_video.h:1185
   pragma Import (C, SDL_GL_CreateContext, "SDL_GL_CreateContext");

  --*
  -- *  \brief Set up an OpenGL context for rendering into an OpenGL window.
  -- *
  -- *  \note The context must have been created with a compatible window.
  --  

   function SDL_GL_MakeCurrent (window : access SDL_Window; context : SDL_GLContext) return int;  -- ..\SDL2_tmp\SDL_video.h:1193
   pragma Import (C, SDL_GL_MakeCurrent, "SDL_GL_MakeCurrent");

  --*
  -- *  \brief Get the currently active OpenGL window.
  --  

   function SDL_GL_GetCurrentWindow return access SDL_Window;  -- ..\SDL2_tmp\SDL_video.h:1199
   pragma Import (C, SDL_GL_GetCurrentWindow, "SDL_GL_GetCurrentWindow");

  --*
  -- *  \brief Get the currently active OpenGL context.
  --  

   function SDL_GL_GetCurrentContext return SDL_GLContext;  -- ..\SDL2_tmp\SDL_video.h:1204
   pragma Import (C, SDL_GL_GetCurrentContext, "SDL_GL_GetCurrentContext");

  --*
  -- *  \brief Get the size of a window's underlying drawable in pixels (for use
  -- *         with glViewport).
  -- *
  -- *  \param window   Window from which the drawable size should be queried
  -- *  \param w        Pointer to variable for storing the width in pixels, may be NULL
  -- *  \param h        Pointer to variable for storing the height in pixels, may be NULL
  -- *
  -- * This may differ from SDL_GetWindowSize() if we're rendering to a high-DPI
  -- * drawable, i.e. the window was created with SDL_WINDOW_ALLOW_HIGHDPI on a
  -- * platform with high-DPI support (Apple calls this "Retina"), and not disabled
  -- * by the SDL_HINT_VIDEO_HIGHDPI_DISABLED hint.
  -- *
  -- *  \sa SDL_GetWindowSize()
  -- *  \sa SDL_CreateWindow()
  --  

   procedure SDL_GL_GetDrawableSize
     (window : access SDL_Window;
      w : access int;
      h : access int);  -- ..\SDL2_tmp\SDL_video.h:1222
   pragma Import (C, SDL_GL_GetDrawableSize, "SDL_GL_GetDrawableSize");

  --*
  -- *  \brief Set the swap interval for the current OpenGL context.
  -- *
  -- *  \param interval 0 for immediate updates, 1 for updates synchronized with the
  -- *                  vertical retrace. If the system supports it, you may
  -- *                  specify -1 to allow late swaps to happen immediately
  -- *                  instead of waiting for the next retrace.
  -- *
  -- *  \return 0 on success, or -1 if setting the swap interval is not supported.
  -- *
  -- *  \sa SDL_GL_GetSwapInterval()
  --  

   function SDL_GL_SetSwapInterval (interval : int) return int;  -- ..\SDL2_tmp\SDL_video.h:1237
   pragma Import (C, SDL_GL_SetSwapInterval, "SDL_GL_SetSwapInterval");

  --*
  -- *  \brief Get the swap interval for the current OpenGL context.
  -- *
  -- *  \return 0 if there is no vertical retrace synchronization, 1 if the buffer
  -- *          swap is synchronized with the vertical retrace, and -1 if late
  -- *          swaps happen immediately instead of waiting for the next retrace.
  -- *          If the system can't determine the swap interval, or there isn't a
  -- *          valid current context, this will return 0 as a safe default.
  -- *
  -- *  \sa SDL_GL_SetSwapInterval()
  --  

   function SDL_GL_GetSwapInterval return int;  -- ..\SDL2_tmp\SDL_video.h:1250
   pragma Import (C, SDL_GL_GetSwapInterval, "SDL_GL_GetSwapInterval");

  --*
  -- * \brief Swap the OpenGL buffers for a window, if double-buffering is
  -- *        supported.
  --  

   procedure SDL_GL_SwapWindow (window : access SDL_Window);  -- ..\SDL2_tmp\SDL_video.h:1256
   pragma Import (C, SDL_GL_SwapWindow, "SDL_GL_SwapWindow");

  --*
  -- *  \brief Delete an OpenGL context.
  -- *
  -- *  \sa SDL_GL_CreateContext()
  --  

   procedure SDL_GL_DeleteContext (context : SDL_GLContext);  -- ..\SDL2_tmp\SDL_video.h:1263
   pragma Import (C, SDL_GL_DeleteContext, "SDL_GL_DeleteContext");

  -- @}  
  -- OpenGL support functions  
  -- Ends C function definitions when using C++  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_video_h;
