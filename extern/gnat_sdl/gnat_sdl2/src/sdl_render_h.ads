pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_stdinc_h;
with Interfaces.C.Strings;
with System;
limited with SDL_video_h;
limited with SDL_surface_h;
with SDL_blendmode_h;
limited with SDL_rect_h;

package SDL_render_h is

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
  -- *  \file SDL_render.h
  -- *
  -- *  Header file for SDL 2D rendering functions.
  -- *
  -- *  This API supports the following features:
  -- *      * single pixel points
  -- *      * single pixel lines
  -- *      * filled rectangles
  -- *      * texture images
  -- *
  -- *  The primitives may be drawn in opaque, blended, or additive modes.
  -- *
  -- *  The texture images may be drawn in opaque, blended, or additive modes.
  -- *  They can have an additional color tint or alpha modulation applied to
  -- *  them, and may also be stretched with linear interpolation.
  -- *
  -- *  This API is designed to accelerate simple 2D operations. You may
  -- *  want more functionality such as polygons and particle effects and
  -- *  in that case you should use SDL's OpenGL/Direct3D support or one
  -- *  of the many good 3D engines.
  -- *
  -- *  These functions must be called from the main thread.
  -- *  See this bug for details: http://bugzilla.libsdl.org/show_bug.cgi?id=1995
  --  

  -- Set up for C function definitions, even when using C++  
  --*
  -- *  \brief Flags used when creating a rendering context
  --  

  --*< The renderer is a software fallback  
  --*< The renderer uses hardware
  --                                                     acceleration  

  --*< Present is synchronized
  --                                                     with the refresh rate  

  --*< The renderer supports
  --                                                     rendering to texture  

   subtype SDL_RendererFlags is unsigned;
   SDL_RENDERER_SOFTWARE : constant unsigned := 1;
   SDL_RENDERER_ACCELERATED : constant unsigned := 2;
   SDL_RENDERER_PRESENTVSYNC : constant unsigned := 4;
   SDL_RENDERER_TARGETTEXTURE : constant unsigned := 8;  -- ..\SDL2_tmp\SDL_render.h:73

  --*
  -- *  \brief Information on the capabilities of a render driver or context.
  --  

  --*< The name of the renderer  
   type SDL_RendererInfo_texture_formats_array is array (0 .. 15) of aliased SDL_stdinc_h.Uint32;
   type SDL_RendererInfo is record
      name : Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_render.h:80
      flags : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_render.h:81
      num_texture_formats : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_render.h:82
      texture_formats : aliased SDL_RendererInfo_texture_formats_array;  -- ..\SDL2_tmp\SDL_render.h:83
      max_texture_width : aliased int;  -- ..\SDL2_tmp\SDL_render.h:84
      max_texture_height : aliased int;  -- ..\SDL2_tmp\SDL_render.h:85
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_RendererInfo);  -- ..\SDL2_tmp\SDL_render.h:78

  --*< Supported ::SDL_RendererFlags  
  --*< The number of available texture formats  
  --*< The available texture formats  
  --*< The maximum texture width  
  --*< The maximum texture height  
  --*
  -- *  \brief The access pattern allowed for a texture.
  --  

  --*< Changes rarely, not lockable  
  --*< Changes frequently, lockable  
  --*< Texture can be used as a render target  
   type SDL_TextureAccess is 
     (SDL_TEXTUREACCESS_STATIC,
      SDL_TEXTUREACCESS_STREAMING,
      SDL_TEXTUREACCESS_TARGET);
   pragma Convention (C, SDL_TextureAccess);  -- ..\SDL2_tmp\SDL_render.h:96

  --*
  -- *  \brief The texture channel modulation used in SDL_RenderCopy().
  --  

  --*< No modulation  
  --*< srcC = srcC * color  
  --*< srcA = srcA * alpha  
   type SDL_TextureModulate is 
     (SDL_TEXTUREMODULATE_NONE,
      SDL_TEXTUREMODULATE_COLOR,
      SDL_TEXTUREMODULATE_ALPHA);
   pragma Convention (C, SDL_TextureModulate);  -- ..\SDL2_tmp\SDL_render.h:106

  --*
  -- *  \brief Flip constants for SDL_RenderCopyEx
  --  

  --*< Do not flip  
  --*< flip horizontally  
  --*< flip vertically  
   type SDL_RendererFlip is 
     (SDL_FLIP_NONE,
      SDL_FLIP_HORIZONTAL,
      SDL_FLIP_VERTICAL);
   pragma Convention (C, SDL_RendererFlip);  -- ..\SDL2_tmp\SDL_render.h:116

  --*
  -- *  \brief A structure representing rendering state
  --  

   type SDL_Renderer is null record;   -- incomplete struct

  --*
  -- *  \brief An efficient driver-specific representation of pixel data
  --  

   type SDL_Texture is null record;   -- incomplete struct

  -- Function prototypes  
  --*
  -- *  \brief Get the number of 2D rendering drivers available for the current
  -- *         display.
  -- *
  -- *  A render driver is a set of code that handles rendering and texture
  -- *  management on a particular display.  Normally there is only one, but
  -- *  some drivers may have several available with different capabilities.
  -- *
  -- *  \sa SDL_GetRenderDriverInfo()
  -- *  \sa SDL_CreateRenderer()
  --  

   function SDL_GetNumRenderDrivers return int;  -- ..\SDL2_tmp\SDL_render.h:144
   pragma Import (C, SDL_GetNumRenderDrivers, "SDL_GetNumRenderDrivers");

  --*
  -- *  \brief Get information about a specific 2D rendering driver for the current
  -- *         display.
  -- *
  -- *  \param index The index of the driver to query information about.
  -- *  \param info  A pointer to an SDL_RendererInfo struct to be filled with
  -- *               information on the rendering driver.
  -- *
  -- *  \return 0 on success, -1 if the index was out of range.
  -- *
  -- *  \sa SDL_CreateRenderer()
  --  

   function SDL_GetRenderDriverInfo (index : int; info : access SDL_RendererInfo) return int;  -- ..\SDL2_tmp\SDL_render.h:158
   pragma Import (C, SDL_GetRenderDriverInfo, "SDL_GetRenderDriverInfo");

  --*
  -- *  \brief Create a window and default renderer
  -- *
  -- *  \param width    The width of the window
  -- *  \param height   The height of the window
  -- *  \param window_flags The flags used to create the window
  -- *  \param window   A pointer filled with the window, or NULL on error
  -- *  \param renderer A pointer filled with the renderer, or NULL on error
  -- *
  -- *  \return 0 on success, or -1 on error
  --  

   function SDL_CreateWindowAndRenderer
     (width : int;
      height : int;
      window_flags : SDL_stdinc_h.Uint32;
      window : System.Address;
      renderer : System.Address) return int;  -- ..\SDL2_tmp\SDL_render.h:172
   pragma Import (C, SDL_CreateWindowAndRenderer, "SDL_CreateWindowAndRenderer");

  --*
  -- *  \brief Create a 2D rendering context for a window.
  -- *
  -- *  \param window The window where rendering is displayed.
  -- *  \param index    The index of the rendering driver to initialize, or -1 to
  -- *                  initialize the first one supporting the requested flags.
  -- *  \param flags    ::SDL_RendererFlags.
  -- *
  -- *  \return A valid rendering context or NULL if there was an error.
  -- *
  -- *  \sa SDL_CreateSoftwareRenderer()
  -- *  \sa SDL_GetRendererInfo()
  -- *  \sa SDL_DestroyRenderer()
  --  

   function SDL_CreateRenderer
     (window : access SDL_video_h.Class_SDL_Window.SDL_Window;
      index : int;
      flags : SDL_stdinc_h.Uint32) return access SDL_Renderer;  -- ..\SDL2_tmp\SDL_render.h:191
   pragma Import (C, SDL_CreateRenderer, "SDL_CreateRenderer");

  --*
  -- *  \brief Create a 2D software rendering context for a surface.
  -- *
  -- *  \param surface The surface where rendering is done.
  -- *
  -- *  \return A valid rendering context or NULL if there was an error.
  -- *
  -- *  \sa SDL_CreateRenderer()
  -- *  \sa SDL_DestroyRenderer()
  --  

   function SDL_CreateSoftwareRenderer (surface : access SDL_surface_h.SDL_Surface) return access SDL_Renderer;  -- ..\SDL2_tmp\SDL_render.h:204
   pragma Import (C, SDL_CreateSoftwareRenderer, "SDL_CreateSoftwareRenderer");

  --*
  -- *  \brief Get the renderer associated with a window.
  --  

   function SDL_GetRenderer (window : access SDL_video_h.Class_SDL_Window.SDL_Window) return access SDL_Renderer;  -- ..\SDL2_tmp\SDL_render.h:209
   pragma Import (C, SDL_GetRenderer, "SDL_GetRenderer");

  --*
  -- *  \brief Get information about a rendering context.
  --  

   function SDL_GetRendererInfo (renderer : access SDL_Renderer; info : access SDL_RendererInfo) return int;  -- ..\SDL2_tmp\SDL_render.h:214
   pragma Import (C, SDL_GetRendererInfo, "SDL_GetRendererInfo");

  --*
  -- *  \brief Get the output size in pixels of a rendering context.
  --  

   function SDL_GetRendererOutputSize
     (renderer : access SDL_Renderer;
      w : access int;
      h : access int) return int;  -- ..\SDL2_tmp\SDL_render.h:220
   pragma Import (C, SDL_GetRendererOutputSize, "SDL_GetRendererOutputSize");

  --*
  -- *  \brief Create a texture for a rendering context.
  -- *
  -- *  \param renderer The renderer.
  -- *  \param format The format of the texture.
  -- *  \param access One of the enumerated values in ::SDL_TextureAccess.
  -- *  \param w      The width of the texture in pixels.
  -- *  \param h      The height of the texture in pixels.
  -- *
  -- *  \return The created texture is returned, or NULL if no rendering context was
  -- *          active,  the format was unsupported, or the width or height were out
  -- *          of range.
  -- *
  -- *  \note The contents of the texture are not defined at creation.
  -- *
  -- *  \sa SDL_QueryTexture()
  -- *  \sa SDL_UpdateTexture()
  -- *  \sa SDL_DestroyTexture()
  --  

   function SDL_CreateTexture
     (renderer : access SDL_Renderer;
      format : SDL_stdinc_h.Uint32;
      c_access : int;
      w : int;
      h : int) return access SDL_Texture;  -- ..\SDL2_tmp\SDL_render.h:242
   pragma Import (C, SDL_CreateTexture, "SDL_CreateTexture");

  --*
  -- *  \brief Create a texture from an existing surface.
  -- *
  -- *  \param renderer The renderer.
  -- *  \param surface The surface containing pixel data used to fill the texture.
  -- *
  -- *  \return The created texture is returned, or NULL on error.
  -- *
  -- *  \note The surface is not modified or freed by this function.
  -- *
  -- *  \sa SDL_QueryTexture()
  -- *  \sa SDL_DestroyTexture()
  --  

   function SDL_CreateTextureFromSurface (renderer : access SDL_Renderer; surface : access SDL_surface_h.SDL_Surface) return access SDL_Texture;  -- ..\SDL2_tmp\SDL_render.h:260
   pragma Import (C, SDL_CreateTextureFromSurface, "SDL_CreateTextureFromSurface");

  --*
  -- *  \brief Query the attributes of a texture
  -- *
  -- *  \param texture A texture to be queried.
  -- *  \param format  A pointer filled in with the raw format of the texture.  The
  -- *                 actual format may differ, but pixel transfers will use this
  -- *                 format.
  -- *  \param access  A pointer filled in with the actual access to the texture.
  -- *  \param w       A pointer filled in with the width of the texture in pixels.
  -- *  \param h       A pointer filled in with the height of the texture in pixels.
  -- *
  -- *  \return 0 on success, or -1 if the texture is not valid.
  --  

   function SDL_QueryTexture
     (texture : access SDL_Texture;
      format : access SDL_stdinc_h.Uint32;
      c_access : access int;
      w : access int;
      h : access int) return int;  -- ..\SDL2_tmp\SDL_render.h:275
   pragma Import (C, SDL_QueryTexture, "SDL_QueryTexture");

  --*
  -- *  \brief Set an additional color value used in render copy operations.
  -- *
  -- *  \param texture The texture to update.
  -- *  \param r       The red color value multiplied into copy operations.
  -- *  \param g       The green color value multiplied into copy operations.
  -- *  \param b       The blue color value multiplied into copy operations.
  -- *
  -- *  \return 0 on success, or -1 if the texture is not valid or color modulation
  -- *          is not supported.
  -- *
  -- *  \sa SDL_GetTextureColorMod()
  --  

   function SDL_SetTextureColorMod
     (texture : access SDL_Texture;
      r : SDL_stdinc_h.Uint8;
      g : SDL_stdinc_h.Uint8;
      b : SDL_stdinc_h.Uint8) return int;  -- ..\SDL2_tmp\SDL_render.h:292
   pragma Import (C, SDL_SetTextureColorMod, "SDL_SetTextureColorMod");

  --*
  -- *  \brief Get the additional color value used in render copy operations.
  -- *
  -- *  \param texture The texture to query.
  -- *  \param r         A pointer filled in with the current red color value.
  -- *  \param g         A pointer filled in with the current green color value.
  -- *  \param b         A pointer filled in with the current blue color value.
  -- *
  -- *  \return 0 on success, or -1 if the texture is not valid.
  -- *
  -- *  \sa SDL_SetTextureColorMod()
  --  

   function SDL_GetTextureColorMod
     (texture : access SDL_Texture;
      r : access SDL_stdinc_h.Uint8;
      g : access SDL_stdinc_h.Uint8;
      b : access SDL_stdinc_h.Uint8) return int;  -- ..\SDL2_tmp\SDL_render.h:308
   pragma Import (C, SDL_GetTextureColorMod, "SDL_GetTextureColorMod");

  --*
  -- *  \brief Set an additional alpha value used in render copy operations.
  -- *
  -- *  \param texture The texture to update.
  -- *  \param alpha     The alpha value multiplied into copy operations.
  -- *
  -- *  \return 0 on success, or -1 if the texture is not valid or alpha modulation
  -- *          is not supported.
  -- *
  -- *  \sa SDL_GetTextureAlphaMod()
  --  

   function SDL_SetTextureAlphaMod (texture : access SDL_Texture; alpha : SDL_stdinc_h.Uint8) return int;  -- ..\SDL2_tmp\SDL_render.h:323
   pragma Import (C, SDL_SetTextureAlphaMod, "SDL_SetTextureAlphaMod");

  --*
  -- *  \brief Get the additional alpha value used in render copy operations.
  -- *
  -- *  \param texture The texture to query.
  -- *  \param alpha     A pointer filled in with the current alpha value.
  -- *
  -- *  \return 0 on success, or -1 if the texture is not valid.
  -- *
  -- *  \sa SDL_SetTextureAlphaMod()
  --  

   function SDL_GetTextureAlphaMod (texture : access SDL_Texture; alpha : access SDL_stdinc_h.Uint8) return int;  -- ..\SDL2_tmp\SDL_render.h:336
   pragma Import (C, SDL_GetTextureAlphaMod, "SDL_GetTextureAlphaMod");

  --*
  -- *  \brief Set the blend mode used for texture copy operations.
  -- *
  -- *  \param texture The texture to update.
  -- *  \param blendMode ::SDL_BlendMode to use for texture blending.
  -- *
  -- *  \return 0 on success, or -1 if the texture is not valid or the blend mode is
  -- *          not supported.
  -- *
  -- *  \note If the blend mode is not supported, the closest supported mode is
  -- *        chosen.
  -- *
  -- *  \sa SDL_GetTextureBlendMode()
  --  

   function SDL_SetTextureBlendMode (texture : access SDL_Texture; blendMode : SDL_blendmode_h.SDL_BlendMode) return int;  -- ..\SDL2_tmp\SDL_render.h:353
   pragma Import (C, SDL_SetTextureBlendMode, "SDL_SetTextureBlendMode");

  --*
  -- *  \brief Get the blend mode used for texture copy operations.
  -- *
  -- *  \param texture   The texture to query.
  -- *  \param blendMode A pointer filled in with the current blend mode.
  -- *
  -- *  \return 0 on success, or -1 if the texture is not valid.
  -- *
  -- *  \sa SDL_SetTextureBlendMode()
  --  

   function SDL_GetTextureBlendMode (texture : access SDL_Texture; blendMode : access SDL_blendmode_h.SDL_BlendMode) return int;  -- ..\SDL2_tmp\SDL_render.h:366
   pragma Import (C, SDL_GetTextureBlendMode, "SDL_GetTextureBlendMode");

  --*
  -- *  \brief Update the given texture rectangle with new pixel data.
  -- *
  -- *  \param texture   The texture to update
  -- *  \param rect      A pointer to the rectangle of pixels to update, or NULL to
  -- *                   update the entire texture.
  -- *  \param pixels    The raw pixel data in the format of the texture.
  -- *  \param pitch     The number of bytes in a row of pixel data, including padding between lines.
  -- *
  -- *  The pixel data must be in the format of the texture. The pixel format can be
  -- *  queried with SDL_QueryTexture.
  -- *
  -- *  \return 0 on success, or -1 if the texture is not valid.
  -- *
  -- *  \note This is a fairly slow function.
  --  

   function SDL_UpdateTexture
     (texture : access SDL_Texture;
      rect : access constant SDL_rect_h.SDL_Rect;
      pixels : System.Address;
      pitch : int) return int;  -- ..\SDL2_tmp\SDL_render.h:385
   pragma Import (C, SDL_UpdateTexture, "SDL_UpdateTexture");

  --*
  -- *  \brief Update a rectangle within a planar YV12 or IYUV texture with new pixel data.
  -- *
  -- *  \param texture   The texture to update
  -- *  \param rect      A pointer to the rectangle of pixels to update, or NULL to
  -- *                   update the entire texture.
  -- *  \param Yplane    The raw pixel data for the Y plane.
  -- *  \param Ypitch    The number of bytes between rows of pixel data for the Y plane.
  -- *  \param Uplane    The raw pixel data for the U plane.
  -- *  \param Upitch    The number of bytes between rows of pixel data for the U plane.
  -- *  \param Vplane    The raw pixel data for the V plane.
  -- *  \param Vpitch    The number of bytes between rows of pixel data for the V plane.
  -- *
  -- *  \return 0 on success, or -1 if the texture is not valid.
  -- *
  -- *  \note You can use SDL_UpdateTexture() as long as your pixel data is
  -- *        a contiguous block of Y and U/V planes in the proper order, but
  -- *        this function is available if your pixel data is not contiguous.
  --  

   function SDL_UpdateYUVTexture
     (texture : access SDL_Texture;
      rect : access constant SDL_rect_h.SDL_Rect;
      Yplane : access SDL_stdinc_h.Uint8;
      Ypitch : int;
      Uplane : access SDL_stdinc_h.Uint8;
      Upitch : int;
      Vplane : access SDL_stdinc_h.Uint8;
      Vpitch : int) return int;  -- ..\SDL2_tmp\SDL_render.h:408
   pragma Import (C, SDL_UpdateYUVTexture, "SDL_UpdateYUVTexture");

  --*
  -- *  \brief Lock a portion of the texture for write-only pixel access.
  -- *
  -- *  \param texture   The texture to lock for access, which was created with
  -- *                   ::SDL_TEXTUREACCESS_STREAMING.
  -- *  \param rect      A pointer to the rectangle to lock for access. If the rect
  -- *                   is NULL, the entire texture will be locked.
  -- *  \param pixels    This is filled in with a pointer to the locked pixels,
  -- *                   appropriately offset by the locked area.
  -- *  \param pitch     This is filled in with the pitch of the locked pixels.
  -- *
  -- *  \return 0 on success, or -1 if the texture is not valid or was not created with ::SDL_TEXTUREACCESS_STREAMING.
  -- *
  -- *  \sa SDL_UnlockTexture()
  --  

   function SDL_LockTexture
     (texture : access SDL_Texture;
      rect : access constant SDL_rect_h.SDL_Rect;
      pixels : System.Address;
      pitch : access int) return int;  -- ..\SDL2_tmp\SDL_render.h:429
   pragma Import (C, SDL_LockTexture, "SDL_LockTexture");

  --*
  -- *  \brief Unlock a texture, uploading the changes to video memory, if needed.
  -- *
  -- *  \sa SDL_LockTexture()
  --  

   procedure SDL_UnlockTexture (texture : access SDL_Texture);  -- ..\SDL2_tmp\SDL_render.h:438
   pragma Import (C, SDL_UnlockTexture, "SDL_UnlockTexture");

  --*
  -- * \brief Determines whether a window supports the use of render targets
  -- *
  -- * \param renderer The renderer that will be checked
  -- *
  -- * \return SDL_TRUE if supported, SDL_FALSE if not.
  --  

   function SDL_RenderTargetSupported (renderer : access SDL_Renderer) return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_render.h:447
   pragma Import (C, SDL_RenderTargetSupported, "SDL_RenderTargetSupported");

  --*
  -- * \brief Set a texture as the current rendering target.
  -- *
  -- * \param renderer The renderer.
  -- * \param texture The targeted texture, which must be created with the SDL_TEXTUREACCESS_TARGET flag, or NULL for the default render target
  -- *
  -- * \return 0 on success, or -1 on error
  -- *
  -- *  \sa SDL_GetRenderTarget()
  --  

   function SDL_SetRenderTarget (renderer : access SDL_Renderer; texture : access SDL_Texture) return int;  -- ..\SDL2_tmp\SDL_render.h:459
   pragma Import (C, SDL_SetRenderTarget, "SDL_SetRenderTarget");

  --*
  -- * \brief Get the current render target or NULL for the default render target.
  -- *
  -- * \return The current render target
  -- *
  -- *  \sa SDL_SetRenderTarget()
  --  

   function SDL_GetRenderTarget (renderer : access SDL_Renderer) return access SDL_Texture;  -- ..\SDL2_tmp\SDL_render.h:469
   pragma Import (C, SDL_GetRenderTarget, "SDL_GetRenderTarget");

  --*
  -- *  \brief Set device independent resolution for rendering
  -- *
  -- *  \param renderer The renderer for which resolution should be set.
  -- *  \param w      The width of the logical resolution
  -- *  \param h      The height of the logical resolution
  -- *
  -- *  This function uses the viewport and scaling functionality to allow a fixed logical
  -- *  resolution for rendering, regardless of the actual output resolution.  If the actual
  -- *  output resolution doesn't have the same aspect ratio the output rendering will be
  -- *  centered within the output display.
  -- *
  -- *  If the output display is a window, mouse events in the window will be filtered
  -- *  and scaled so they seem to arrive within the logical resolution.
  -- *
  -- *  \note If this function results in scaling or subpixel drawing by the
  -- *        rendering backend, it will be handled using the appropriate
  -- *        quality hints.
  -- *
  -- *  \sa SDL_RenderGetLogicalSize()
  -- *  \sa SDL_RenderSetScale()
  -- *  \sa SDL_RenderSetViewport()
  --  

   function SDL_RenderSetLogicalSize
     (renderer : access SDL_Renderer;
      w : int;
      h : int) return int;  -- ..\SDL2_tmp\SDL_render.h:494
   pragma Import (C, SDL_RenderSetLogicalSize, "SDL_RenderSetLogicalSize");

  --*
  -- *  \brief Get device independent resolution for rendering
  -- *
  -- *  \param renderer The renderer from which resolution should be queried.
  -- *  \param w      A pointer filled with the width of the logical resolution
  -- *  \param h      A pointer filled with the height of the logical resolution
  -- *
  -- *  \sa SDL_RenderSetLogicalSize()
  --  

   procedure SDL_RenderGetLogicalSize
     (renderer : access SDL_Renderer;
      w : access int;
      h : access int);  -- ..\SDL2_tmp\SDL_render.h:505
   pragma Import (C, SDL_RenderGetLogicalSize, "SDL_RenderGetLogicalSize");

  --*
  -- *  \brief Set whether to force integer scales for resolution-independent rendering
  -- *
  -- *  \param renderer The renderer for which integer scaling should be set.
  -- *  \param enable   Enable or disable integer scaling
  -- *
  -- *  This function restricts the logical viewport to integer values - that is, when
  -- *  a resolution is between two multiples of a logical size, the viewport size is
  -- *  rounded down to the lower multiple.
  -- *
  -- *  \sa SDL_RenderSetLogicalSize()
  --  

   function SDL_RenderSetIntegerScale (renderer : access SDL_Renderer; enable : SDL_stdinc_h.SDL_bool) return int;  -- ..\SDL2_tmp\SDL_render.h:519
   pragma Import (C, SDL_RenderSetIntegerScale, "SDL_RenderSetIntegerScale");

  --*
  -- *  \brief Get whether integer scales are forced for resolution-independent rendering
  -- *
  -- *  \param renderer The renderer from which integer scaling should be queried.
  -- *
  -- *  \sa SDL_RenderSetIntegerScale()
  --  

   function SDL_RenderGetIntegerScale (renderer : access SDL_Renderer) return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_render.h:529
   pragma Import (C, SDL_RenderGetIntegerScale, "SDL_RenderGetIntegerScale");

  --*
  -- *  \brief Set the drawing area for rendering on the current target.
  -- *
  -- *  \param renderer The renderer for which the drawing area should be set.
  -- *  \param rect The rectangle representing the drawing area, or NULL to set the viewport to the entire target.
  -- *
  -- *  The x,y of the viewport rect represents the origin for rendering.
  -- *
  -- *  \return 0 on success, or -1 on error
  -- *
  -- *  \note If the window associated with the renderer is resized, the viewport is automatically reset.
  -- *
  -- *  \sa SDL_RenderGetViewport()
  -- *  \sa SDL_RenderSetLogicalSize()
  --  

   function SDL_RenderSetViewport (renderer : access SDL_Renderer; rect : access constant SDL_rect_h.SDL_Rect) return int;  -- ..\SDL2_tmp\SDL_render.h:546
   pragma Import (C, SDL_RenderSetViewport, "SDL_RenderSetViewport");

  --*
  -- *  \brief Get the drawing area for the current target.
  -- *
  -- *  \sa SDL_RenderSetViewport()
  --  

   procedure SDL_RenderGetViewport (renderer : access SDL_Renderer; rect : access SDL_rect_h.SDL_Rect);  -- ..\SDL2_tmp\SDL_render.h:554
   pragma Import (C, SDL_RenderGetViewport, "SDL_RenderGetViewport");

  --*
  -- *  \brief Set the clip rectangle for the current target.
  -- *
  -- *  \param renderer The renderer for which clip rectangle should be set.
  -- *  \param rect   A pointer to the rectangle to set as the clip rectangle, or
  -- *                NULL to disable clipping.
  -- *
  -- *  \return 0 on success, or -1 on error
  -- *
  -- *  \sa SDL_RenderGetClipRect()
  --  

   function SDL_RenderSetClipRect (renderer : access SDL_Renderer; rect : access constant SDL_rect_h.SDL_Rect) return int;  -- ..\SDL2_tmp\SDL_render.h:568
   pragma Import (C, SDL_RenderSetClipRect, "SDL_RenderSetClipRect");

  --*
  -- *  \brief Get the clip rectangle for the current target.
  -- *
  -- *  \param renderer The renderer from which clip rectangle should be queried.
  -- *  \param rect   A pointer filled in with the current clip rectangle, or
  -- *                an empty rectangle if clipping is disabled.
  -- *
  -- *  \sa SDL_RenderSetClipRect()
  --  

   procedure SDL_RenderGetClipRect (renderer : access SDL_Renderer; rect : access SDL_rect_h.SDL_Rect);  -- ..\SDL2_tmp\SDL_render.h:580
   pragma Import (C, SDL_RenderGetClipRect, "SDL_RenderGetClipRect");

  --*
  -- *  \brief Get whether clipping is enabled on the given renderer.
  -- *
  -- *  \param renderer The renderer from which clip state should be queried.
  -- *
  -- *  \sa SDL_RenderGetClipRect()
  --  

   function SDL_RenderIsClipEnabled (renderer : access SDL_Renderer) return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_render.h:590
   pragma Import (C, SDL_RenderIsClipEnabled, "SDL_RenderIsClipEnabled");

  --*
  -- *  \brief Set the drawing scale for rendering on the current target.
  -- *
  -- *  \param renderer The renderer for which the drawing scale should be set.
  -- *  \param scaleX The horizontal scaling factor
  -- *  \param scaleY The vertical scaling factor
  -- *
  -- *  The drawing coordinates are scaled by the x/y scaling factors
  -- *  before they are used by the renderer.  This allows resolution
  -- *  independent drawing with a single coordinate system.
  -- *
  -- *  \note If this results in scaling or subpixel drawing by the
  -- *        rendering backend, it will be handled using the appropriate
  -- *        quality hints.  For best results use integer scaling factors.
  -- *
  -- *  \sa SDL_RenderGetScale()
  -- *  \sa SDL_RenderSetLogicalSize()
  --  

   function SDL_RenderSetScale
     (renderer : access SDL_Renderer;
      scaleX : float;
      scaleY : float) return int;  -- ..\SDL2_tmp\SDL_render.h:611
   pragma Import (C, SDL_RenderSetScale, "SDL_RenderSetScale");

  --*
  -- *  \brief Get the drawing scale for the current target.
  -- *
  -- *  \param renderer The renderer from which drawing scale should be queried.
  -- *  \param scaleX A pointer filled in with the horizontal scaling factor
  -- *  \param scaleY A pointer filled in with the vertical scaling factor
  -- *
  -- *  \sa SDL_RenderSetScale()
  --  

   procedure SDL_RenderGetScale
     (renderer : access SDL_Renderer;
      scaleX : access float;
      scaleY : access float);  -- ..\SDL2_tmp\SDL_render.h:623
   pragma Import (C, SDL_RenderGetScale, "SDL_RenderGetScale");

  --*
  -- *  \brief Set the color used for drawing operations (Rect, Line and Clear).
  -- *
  -- *  \param renderer The renderer for which drawing color should be set.
  -- *  \param r The red value used to draw on the rendering target.
  -- *  \param g The green value used to draw on the rendering target.
  -- *  \param b The blue value used to draw on the rendering target.
  -- *  \param a The alpha value used to draw on the rendering target, usually
  -- *           ::SDL_ALPHA_OPAQUE (255).
  -- *
  -- *  \return 0 on success, or -1 on error
  --  

   function SDL_SetRenderDrawColor
     (renderer : access SDL_Renderer;
      r : SDL_stdinc_h.Uint8;
      g : SDL_stdinc_h.Uint8;
      b : SDL_stdinc_h.Uint8;
      a : SDL_stdinc_h.Uint8) return int;  -- ..\SDL2_tmp\SDL_render.h:638
   pragma Import (C, SDL_SetRenderDrawColor, "SDL_SetRenderDrawColor");

  --*
  -- *  \brief Get the color used for drawing operations (Rect, Line and Clear).
  -- *
  -- *  \param renderer The renderer from which drawing color should be queried.
  -- *  \param r A pointer to the red value used to draw on the rendering target.
  -- *  \param g A pointer to the green value used to draw on the rendering target.
  -- *  \param b A pointer to the blue value used to draw on the rendering target.
  -- *  \param a A pointer to the alpha value used to draw on the rendering target,
  -- *           usually ::SDL_ALPHA_OPAQUE (255).
  -- *
  -- *  \return 0 on success, or -1 on error
  --  

   function SDL_GetRenderDrawColor
     (renderer : access SDL_Renderer;
      r : access SDL_stdinc_h.Uint8;
      g : access SDL_stdinc_h.Uint8;
      b : access SDL_stdinc_h.Uint8;
      a : access SDL_stdinc_h.Uint8) return int;  -- ..\SDL2_tmp\SDL_render.h:654
   pragma Import (C, SDL_GetRenderDrawColor, "SDL_GetRenderDrawColor");

  --*
  -- *  \brief Set the blend mode used for drawing operations (Fill and Line).
  -- *
  -- *  \param renderer The renderer for which blend mode should be set.
  -- *  \param blendMode ::SDL_BlendMode to use for blending.
  -- *
  -- *  \return 0 on success, or -1 on error
  -- *
  -- *  \note If the blend mode is not supported, the closest supported mode is
  -- *        chosen.
  -- *
  -- *  \sa SDL_GetRenderDrawBlendMode()
  --  

   function SDL_SetRenderDrawBlendMode (renderer : access SDL_Renderer; blendMode : SDL_blendmode_h.SDL_BlendMode) return int;  -- ..\SDL2_tmp\SDL_render.h:671
   pragma Import (C, SDL_SetRenderDrawBlendMode, "SDL_SetRenderDrawBlendMode");

  --*
  -- *  \brief Get the blend mode used for drawing operations.
  -- *
  -- *  \param renderer The renderer from which blend mode should be queried.
  -- *  \param blendMode A pointer filled in with the current blend mode.
  -- *
  -- *  \return 0 on success, or -1 on error
  -- *
  -- *  \sa SDL_SetRenderDrawBlendMode()
  --  

   function SDL_GetRenderDrawBlendMode (renderer : access SDL_Renderer; blendMode : access SDL_blendmode_h.SDL_BlendMode) return int;  -- ..\SDL2_tmp\SDL_render.h:684
   pragma Import (C, SDL_GetRenderDrawBlendMode, "SDL_GetRenderDrawBlendMode");

  --*
  -- *  \brief Clear the current rendering target with the drawing color
  -- *
  -- *  This function clears the entire rendering target, ignoring the viewport and
  -- *  the clip rectangle.
  -- *
  -- *  \return 0 on success, or -1 on error
  --  

   function SDL_RenderClear (renderer : access SDL_Renderer) return int;  -- ..\SDL2_tmp\SDL_render.h:695
   pragma Import (C, SDL_RenderClear, "SDL_RenderClear");

  --*
  -- *  \brief Draw a point on the current rendering target.
  -- *
  -- *  \param renderer The renderer which should draw a point.
  -- *  \param x The x coordinate of the point.
  -- *  \param y The y coordinate of the point.
  -- *
  -- *  \return 0 on success, or -1 on error
  --  

   function SDL_RenderDrawPoint
     (renderer : access SDL_Renderer;
      x : int;
      y : int) return int;  -- ..\SDL2_tmp\SDL_render.h:706
   pragma Import (C, SDL_RenderDrawPoint, "SDL_RenderDrawPoint");

  --*
  -- *  \brief Draw multiple points on the current rendering target.
  -- *
  -- *  \param renderer The renderer which should draw multiple points.
  -- *  \param points The points to draw
  -- *  \param count The number of points to draw
  -- *
  -- *  \return 0 on success, or -1 on error
  --  

   function SDL_RenderDrawPoints
     (renderer : access SDL_Renderer;
      points : access constant SDL_rect_h.SDL_Point;
      count : int) return int;  -- ..\SDL2_tmp\SDL_render.h:718
   pragma Import (C, SDL_RenderDrawPoints, "SDL_RenderDrawPoints");

  --*
  -- *  \brief Draw a line on the current rendering target.
  -- *
  -- *  \param renderer The renderer which should draw a line.
  -- *  \param x1 The x coordinate of the start point.
  -- *  \param y1 The y coordinate of the start point.
  -- *  \param x2 The x coordinate of the end point.
  -- *  \param y2 The y coordinate of the end point.
  -- *
  -- *  \return 0 on success, or -1 on error
  --  

   function SDL_RenderDrawLine
     (renderer : access SDL_Renderer;
      x1 : int;
      y1 : int;
      x2 : int;
      y2 : int) return int;  -- ..\SDL2_tmp\SDL_render.h:733
   pragma Import (C, SDL_RenderDrawLine, "SDL_RenderDrawLine");

  --*
  -- *  \brief Draw a series of connected lines on the current rendering target.
  -- *
  -- *  \param renderer The renderer which should draw multiple lines.
  -- *  \param points The points along the lines
  -- *  \param count The number of points, drawing count-1 lines
  -- *
  -- *  \return 0 on success, or -1 on error
  --  

   function SDL_RenderDrawLines
     (renderer : access SDL_Renderer;
      points : access constant SDL_rect_h.SDL_Point;
      count : int) return int;  -- ..\SDL2_tmp\SDL_render.h:745
   pragma Import (C, SDL_RenderDrawLines, "SDL_RenderDrawLines");

  --*
  -- *  \brief Draw a rectangle on the current rendering target.
  -- *
  -- *  \param renderer The renderer which should draw a rectangle.
  -- *  \param rect A pointer to the destination rectangle, or NULL to outline the entire rendering target.
  -- *
  -- *  \return 0 on success, or -1 on error
  --  

   function SDL_RenderDrawRect (renderer : access SDL_Renderer; rect : access constant SDL_rect_h.SDL_Rect) return int;  -- ..\SDL2_tmp\SDL_render.h:757
   pragma Import (C, SDL_RenderDrawRect, "SDL_RenderDrawRect");

  --*
  -- *  \brief Draw some number of rectangles on the current rendering target.
  -- *
  -- *  \param renderer The renderer which should draw multiple rectangles.
  -- *  \param rects A pointer to an array of destination rectangles.
  -- *  \param count The number of rectangles.
  -- *
  -- *  \return 0 on success, or -1 on error
  --  

   function SDL_RenderDrawRects
     (renderer : access SDL_Renderer;
      rects : access constant SDL_rect_h.SDL_Rect;
      count : int) return int;  -- ..\SDL2_tmp\SDL_render.h:769
   pragma Import (C, SDL_RenderDrawRects, "SDL_RenderDrawRects");

  --*
  -- *  \brief Fill a rectangle on the current rendering target with the drawing color.
  -- *
  -- *  \param renderer The renderer which should fill a rectangle.
  -- *  \param rect A pointer to the destination rectangle, or NULL for the entire
  -- *              rendering target.
  -- *
  -- *  \return 0 on success, or -1 on error
  --  

   function SDL_RenderFillRect (renderer : access SDL_Renderer; rect : access constant SDL_rect_h.SDL_Rect) return int;  -- ..\SDL2_tmp\SDL_render.h:782
   pragma Import (C, SDL_RenderFillRect, "SDL_RenderFillRect");

  --*
  -- *  \brief Fill some number of rectangles on the current rendering target with the drawing color.
  -- *
  -- *  \param renderer The renderer which should fill multiple rectangles.
  -- *  \param rects A pointer to an array of destination rectangles.
  -- *  \param count The number of rectangles.
  -- *
  -- *  \return 0 on success, or -1 on error
  --  

   function SDL_RenderFillRects
     (renderer : access SDL_Renderer;
      rects : access constant SDL_rect_h.SDL_Rect;
      count : int) return int;  -- ..\SDL2_tmp\SDL_render.h:794
   pragma Import (C, SDL_RenderFillRects, "SDL_RenderFillRects");

  --*
  -- *  \brief Copy a portion of the texture to the current rendering target.
  -- *
  -- *  \param renderer The renderer which should copy parts of a texture.
  -- *  \param texture The source texture.
  -- *  \param srcrect   A pointer to the source rectangle, or NULL for the entire
  -- *                   texture.
  -- *  \param dstrect   A pointer to the destination rectangle, or NULL for the
  -- *                   entire rendering target.
  -- *
  -- *  \return 0 on success, or -1 on error
  --  

   function SDL_RenderCopy
     (renderer : access SDL_Renderer;
      texture : access SDL_Texture;
      srcrect : access constant SDL_rect_h.SDL_Rect;
      dstrect : access constant SDL_rect_h.SDL_Rect) return int;  -- ..\SDL2_tmp\SDL_render.h:810
   pragma Import (C, SDL_RenderCopy, "SDL_RenderCopy");

  --*
  -- *  \brief Copy a portion of the source texture to the current rendering target, rotating it by angle around the given center
  -- *
  -- *  \param renderer The renderer which should copy parts of a texture.
  -- *  \param texture The source texture.
  -- *  \param srcrect   A pointer to the source rectangle, or NULL for the entire
  -- *                   texture.
  -- *  \param dstrect   A pointer to the destination rectangle, or NULL for the
  -- *                   entire rendering target.
  -- *  \param angle    An angle in degrees that indicates the rotation that will be applied to dstrect, rotating it in a clockwise direction
  -- *  \param center   A pointer to a point indicating the point around which dstrect will be rotated (if NULL, rotation will be done around dstrect.w/2, dstrect.h/2).
  -- *  \param flip     An SDL_RendererFlip value stating which flipping actions should be performed on the texture
  -- *
  -- *  \return 0 on success, or -1 on error
  --  

   function SDL_RenderCopyEx
     (renderer : access SDL_Renderer;
      texture : access SDL_Texture;
      srcrect : access constant SDL_rect_h.SDL_Rect;
      dstrect : access constant SDL_rect_h.SDL_Rect;
      angle : double;
      center : access constant SDL_rect_h.SDL_Point;
      flip : SDL_RendererFlip) return int;  -- ..\SDL2_tmp\SDL_render.h:830
   pragma Import (C, SDL_RenderCopyEx, "SDL_RenderCopyEx");

  --*
  -- *  \brief Read pixels from the current rendering target.
  -- *
  -- *  \param renderer The renderer from which pixels should be read.
  -- *  \param rect   A pointer to the rectangle to read, or NULL for the entire
  -- *                render target.
  -- *  \param format The desired format of the pixel data, or 0 to use the format
  -- *                of the rendering target
  -- *  \param pixels A pointer to be filled in with the pixel data
  -- *  \param pitch  The pitch of the pixels parameter.
  -- *
  -- *  \return 0 on success, or -1 if pixel reading is not supported.
  -- *
  -- *  \warning This is a very slow operation, and should not be used frequently.
  --  

   function SDL_RenderReadPixels
     (renderer : access SDL_Renderer;
      rect : access constant SDL_rect_h.SDL_Rect;
      format : SDL_stdinc_h.Uint32;
      pixels : System.Address;
      pitch : int) return int;  -- ..\SDL2_tmp\SDL_render.h:853
   pragma Import (C, SDL_RenderReadPixels, "SDL_RenderReadPixels");

  --*
  -- *  \brief Update the screen with rendering performed.
  --  

   procedure SDL_RenderPresent (renderer : access SDL_Renderer);  -- ..\SDL2_tmp\SDL_render.h:861
   pragma Import (C, SDL_RenderPresent, "SDL_RenderPresent");

  --*
  -- *  \brief Destroy the specified texture.
  -- *
  -- *  \sa SDL_CreateTexture()
  -- *  \sa SDL_CreateTextureFromSurface()
  --  

   procedure SDL_DestroyTexture (texture : access SDL_Texture);  -- ..\SDL2_tmp\SDL_render.h:869
   pragma Import (C, SDL_DestroyTexture, "SDL_DestroyTexture");

  --*
  -- *  \brief Destroy the rendering context for a window and free associated
  -- *         textures.
  -- *
  -- *  \sa SDL_CreateRenderer()
  --  

   procedure SDL_DestroyRenderer (renderer : access SDL_Renderer);  -- ..\SDL2_tmp\SDL_render.h:877
   pragma Import (C, SDL_DestroyRenderer, "SDL_DestroyRenderer");

  --*
  -- *  \brief Bind the texture to the current OpenGL/ES/ES2 context for use with
  -- *         OpenGL instructions.
  -- *
  -- *  \param texture  The SDL texture to bind
  -- *  \param texw     A pointer to a float that will be filled with the texture width
  -- *  \param texh     A pointer to a float that will be filled with the texture height
  -- *
  -- *  \return 0 on success, or -1 if the operation is not supported
  --  

   function SDL_GL_BindTexture
     (texture : access SDL_Texture;
      texw : access float;
      texh : access float) return int;  -- ..\SDL2_tmp\SDL_render.h:890
   pragma Import (C, SDL_GL_BindTexture, "SDL_GL_BindTexture");

  --*
  -- *  \brief Unbind a texture from the current OpenGL/ES/ES2 context.
  -- *
  -- *  \param texture  The SDL texture to unbind
  -- *
  -- *  \return 0 on success, or -1 if the operation is not supported
  --  

   function SDL_GL_UnbindTexture (texture : access SDL_Texture) return int;  -- ..\SDL2_tmp\SDL_render.h:899
   pragma Import (C, SDL_GL_UnbindTexture, "SDL_GL_UnbindTexture");

  --*
  -- *  \brief Get the CAMetalLayer associated with the given Metal renderer
  -- *
  -- *  \param renderer The renderer to query
  -- *
  -- *  \return CAMetalLayer* on success, or NULL if the renderer isn't a Metal renderer
  -- *
  -- *  \sa SDL_RenderGetMetalCommandEncoder()
  --  

   function SDL_RenderGetMetalLayer (renderer : access SDL_Renderer) return System.Address;  -- ..\SDL2_tmp\SDL_render.h:910
   pragma Import (C, SDL_RenderGetMetalLayer, "SDL_RenderGetMetalLayer");

  --*
  -- *  \brief Get the Metal command encoder for the current frame
  -- *
  -- *  \param renderer The renderer to query
  -- *
  -- *  \return id<MTLRenderCommandEncoder> on success, or NULL if the renderer isn't a Metal renderer
  -- *
  -- *  \sa SDL_RenderGetMetalLayer()
  --  

   function SDL_RenderGetMetalCommandEncoder (renderer : access SDL_Renderer) return System.Address;  -- ..\SDL2_tmp\SDL_render.h:921
   pragma Import (C, SDL_RenderGetMetalCommandEncoder, "SDL_RenderGetMetalCommandEncoder");

  -- Ends C function definitions when using C++  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_render_h;
