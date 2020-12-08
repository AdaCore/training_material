pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_stdinc_h;
limited with SDL_pixels_h; 
with System;
with SDL_rect_h;
limited with SDL_rwops_h;
with SDL_blendmode_h;

package SDL_surface_h is

   SDL_SWSURFACE : constant := 0;  --  ..\SDL2_tmp\SDL_surface.h:52
   SDL_PREALLOC : constant := 16#00000001#;  --  ..\SDL2_tmp\SDL_surface.h:53
   SDL_RLEACCEL : constant := 16#00000002#;  --  ..\SDL2_tmp\SDL_surface.h:54
   SDL_DONTFREE : constant := 16#00000004#;  --  ..\SDL2_tmp\SDL_surface.h:55
   --  arg-macro: function SDL_MUSTLOCK (S)
   --    return ((S).flags and SDL_RLEACCEL) /= 0;
   --  arg-macro: procedure SDL_LoadBMP (file)
   --    SDL_LoadBMP_RW(SDL_RWFromFile(file, "rb"), 1)
   --  arg-macro: procedure SDL_SaveBMP (surface, file)
   --    SDL_SaveBMP_RW(surface, SDL_RWFromFile(file, "wb"), 1)
   --  unsupported macro: SDL_BlitSurface SDL_UpperBlit
   --  unsupported macro: SDL_BlitScaled SDL_UpperBlitScaled

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
  -- *  \file SDL_surface.h
  -- *
  -- *  Header file for ::SDL_Surface definition and management functions.
  --  

  -- Set up for C function definitions, even when using C++  
  --*
  -- *  \name Surface flags
  -- *
  -- *  These are the currently supported flags for the ::SDL_Surface.
  -- *
  -- *  \internal
  -- *  Used internally (read-only).
  --  

  -- @{  
  -- @}  
  -- Surface flags  
  --*
  -- *  Evaluates to true if the surface needs to be locked before access.
  --  

  --*
  -- * \brief A collection of pixels used in software blitting.
  -- *
  -- * \note  This structure should be treated as read-only, except for \c pixels,
  -- *        which, if not NULL, contains the raw pixel data for the surface.
  --  

  --*< Read-only  
   type SDL_BlitMap;
   type SDL_Surface is record
      flags : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_surface.h:71
      format : access SDL_pixels_h.SDL_PixelFormat;  -- ..\SDL2_tmp\SDL_surface.h:72
      w : aliased int;  -- ..\SDL2_tmp\SDL_surface.h:73
      h : aliased int;  -- ..\SDL2_tmp\SDL_surface.h:73
      pitch : aliased int;  -- ..\SDL2_tmp\SDL_surface.h:74
      pixels : System.Address;  -- ..\SDL2_tmp\SDL_surface.h:75
      userdata : System.Address;  -- ..\SDL2_tmp\SDL_surface.h:78
      locked : aliased int;  -- ..\SDL2_tmp\SDL_surface.h:81
      lock_data : System.Address;  -- ..\SDL2_tmp\SDL_surface.h:82
      clip_rect : aliased SDL_rect_h.SDL_Rect;  -- ..\SDL2_tmp\SDL_surface.h:85
      map : access SDL_BlitMap;  -- ..\SDL2_tmp\SDL_surface.h:88
      refcount : aliased int;  -- ..\SDL2_tmp\SDL_surface.h:91
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_Surface);  -- ..\SDL2_tmp\SDL_surface.h:69

  --*< Read-only  
  --*< Read-only  
  --*< Read-only  
  --*< Read-write  
  --* Application data associated with the surface  
  --*< Read-write  
  --* information needed for surfaces requiring locks  
  --*< Read-only  
  --*< Read-only  
  --* clipping information  
  --*< Read-only  
  --* info for fast blit mapping to other surfaces  
  --*< Private  
   type SDL_BlitMap is null record;   -- incomplete struct

  --* Reference count -- used when freeing surface  
  --*< Read-mostly  
  --*
  -- * \brief The type of function used for surface blitting functions.
  --  

   type SDL_blit is access function
        (arg1 : access SDL_Surface;
         arg2 : access SDL_rect_h.SDL_Rect;
         arg3 : access SDL_Surface;
         arg4 : access SDL_rect_h.SDL_Rect) return int;
   pragma Convention (C, SDL_blit);  -- ..\SDL2_tmp\SDL_surface.h:97

  --*
  -- * \brief The formula used for converting between YUV and RGB
  --  

  --*< Full range JPEG  
  --*< BT.601 (the default)  
  --*< BT.709  
  --*< BT.601 for SD content, BT.709 for HD content  
   type SDL_YUV_CONVERSION_MODE is 
     (SDL_YUV_CONVERSION_JPEG,
      SDL_YUV_CONVERSION_BT601,
      SDL_YUV_CONVERSION_BT709,
      SDL_YUV_CONVERSION_AUTOMATIC);
   pragma Convention (C, SDL_YUV_CONVERSION_MODE);  -- ..\SDL2_tmp\SDL_surface.h:109

  --*
  -- *  Allocate and free an RGB surface.
  -- *
  -- *  If the depth is 4 or 8 bits, an empty palette is allocated for the surface.
  -- *  If the depth is greater than 8 bits, the pixel format is set using the
  -- *  flags '[RGB]mask'.
  -- *
  -- *  If the function runs out of memory, it will return NULL.
  -- *
  -- *  \param flags The \c flags are obsolete and should be set to 0.
  -- *  \param width The width in pixels of the surface to create.
  -- *  \param height The height in pixels of the surface to create.
  -- *  \param depth The depth in bits of the surface to create.
  -- *  \param Rmask The red mask of the surface to create.
  -- *  \param Gmask The green mask of the surface to create.
  -- *  \param Bmask The blue mask of the surface to create.
  -- *  \param Amask The alpha mask of the surface to create.
  --  

   function SDL_CreateRGBSurface
     (flags : SDL_stdinc_h.Uint32;
      width : int;
      height : int;
      depth : int;
      Rmask : SDL_stdinc_h.Uint32;
      Gmask : SDL_stdinc_h.Uint32;
      Bmask : SDL_stdinc_h.Uint32;
      Amask : SDL_stdinc_h.Uint32) return access SDL_Surface;  -- ..\SDL2_tmp\SDL_surface.h:129
   pragma Import (C, SDL_CreateRGBSurface, "SDL_CreateRGBSurface");

  -- !!! FIXME for 2.1: why does this ask for depth? Format provides that.  
   function SDL_CreateRGBSurfaceWithFormat
     (flags : SDL_stdinc_h.Uint32;
      width : int;
      height : int;
      depth : int;
      format : SDL_stdinc_h.Uint32) return access SDL_Surface;  -- ..\SDL2_tmp\SDL_surface.h:134
   pragma Import (C, SDL_CreateRGBSurfaceWithFormat, "SDL_CreateRGBSurfaceWithFormat");

   function SDL_CreateRGBSurfaceFrom
     (pixels : System.Address;
      width : int;
      height : int;
      depth : int;
      pitch : int;
      Rmask : SDL_stdinc_h.Uint32;
      Gmask : SDL_stdinc_h.Uint32;
      Bmask : SDL_stdinc_h.Uint32;
      Amask : SDL_stdinc_h.Uint32) return access SDL_Surface;  -- ..\SDL2_tmp\SDL_surface.h:137
   pragma Import (C, SDL_CreateRGBSurfaceFrom, "SDL_CreateRGBSurfaceFrom");

   function SDL_CreateRGBSurfaceWithFormatFrom
     (pixels : System.Address;
      width : int;
      height : int;
      depth : int;
      pitch : int;
      format : SDL_stdinc_h.Uint32) return access SDL_Surface;  -- ..\SDL2_tmp\SDL_surface.h:146
   pragma Import (C, SDL_CreateRGBSurfaceWithFormatFrom, "SDL_CreateRGBSurfaceWithFormatFrom");

   procedure SDL_FreeSurface (surface : access SDL_Surface);  -- ..\SDL2_tmp\SDL_surface.h:148
   pragma Import (C, SDL_FreeSurface, "SDL_FreeSurface");

  --*
  -- *  \brief Set the palette used by a surface.
  -- *
  -- *  \return 0, or -1 if the surface format doesn't use a palette.
  -- *
  -- *  \note A single palette can be shared with many surfaces.
  --  

   function SDL_SetSurfacePalette (surface : access SDL_Surface; palette : access SDL_pixels_h.SDL_Palette) return int;  -- ..\SDL2_tmp\SDL_surface.h:157
   pragma Import (C, SDL_SetSurfacePalette, "SDL_SetSurfacePalette");

  --*
  -- *  \brief Sets up a surface for directly accessing the pixels.
  -- *
  -- *  Between calls to SDL_LockSurface() / SDL_UnlockSurface(), you can write
  -- *  to and read from \c surface->pixels, using the pixel format stored in
  -- *  \c surface->format.  Once you are done accessing the surface, you should
  -- *  use SDL_UnlockSurface() to release it.
  -- *
  -- *  Not all surfaces require locking.  If SDL_MUSTLOCK(surface) evaluates
  -- *  to 0, then you can read and write to the surface at any time, and the
  -- *  pixel format of the surface will not change.
  -- *
  -- *  No operating system or library calls should be made between lock/unlock
  -- *  pairs, as critical system locks may be held during this time.
  -- *
  -- *  SDL_LockSurface() returns 0, or -1 if the surface couldn't be locked.
  -- *
  -- *  \sa SDL_UnlockSurface()
  --  

   function SDL_LockSurface (surface : access SDL_Surface) return int;  -- ..\SDL2_tmp\SDL_surface.h:179
   pragma Import (C, SDL_LockSurface, "SDL_LockSurface");

  --* \sa SDL_LockSurface()  
   procedure SDL_UnlockSurface (surface : access SDL_Surface);  -- ..\SDL2_tmp\SDL_surface.h:181
   pragma Import (C, SDL_UnlockSurface, "SDL_UnlockSurface");

  --*
  -- *  Load a surface from a seekable SDL data stream (memory or file).
  -- *
  -- *  If \c freesrc is non-zero, the stream will be closed after being read.
  -- *
  -- *  The new surface should be freed with SDL_FreeSurface().
  -- *
  -- *  \return the new surface, or NULL if there was an error.
  --  

   function SDL_LoadBMP_RW (src : access SDL_rwops_h.SDL_RWops; freesrc : int) return access SDL_Surface;  -- ..\SDL2_tmp\SDL_surface.h:192
   pragma Import (C, SDL_LoadBMP_RW, "SDL_LoadBMP_RW");

  --*
  -- *  Load a surface from a file.
  -- *
  -- *  Convenience macro.
  --  

  --*
  -- *  Save a surface to a seekable SDL data stream (memory or file).
  -- *
  -- *  Surfaces with a 24-bit, 32-bit and paletted 8-bit format get saved in the
  -- *  BMP directly. Other RGB formats with 8-bit or higher get converted to a
  -- *  24-bit surface or, if they have an alpha mask or a colorkey, to a 32-bit
  -- *  surface before they are saved. YUV and paletted 1-bit and 4-bit formats are
  -- *  not supported.
  -- *
  -- *  If \c freedst is non-zero, the stream will be closed after being written.
  -- *
  -- *  \return 0 if successful or -1 if there was an error.
  --  

   function SDL_SaveBMP_RW
     (surface : access SDL_Surface;
      dst : access SDL_rwops_h.SDL_RWops;
      freedst : int) return int;  -- ..\SDL2_tmp\SDL_surface.h:215
   pragma Import (C, SDL_SaveBMP_RW, "SDL_SaveBMP_RW");

  --*
  -- *  Save a surface to a file.
  -- *
  -- *  Convenience macro.
  --  

  --*
  -- *  \brief Sets the RLE acceleration hint for a surface.
  -- *
  -- *  \return 0 on success, or -1 if the surface is not valid
  -- *
  -- *  \note If RLE is enabled, colorkey and alpha blending blits are much faster,
  -- *        but the surface must be locked before directly accessing the pixels.
  --  

   function SDL_SetSurfaceRLE (surface : access SDL_Surface; flag : int) return int;  -- ..\SDL2_tmp\SDL_surface.h:234
   pragma Import (C, SDL_SetSurfaceRLE, "SDL_SetSurfaceRLE");

  --*
  -- *  \brief Sets the color key (transparent pixel) in a blittable surface.
  -- *
  -- *  \param surface The surface to update
  -- *  \param flag Non-zero to enable colorkey and 0 to disable colorkey
  -- *  \param key The transparent pixel in the native surface format
  -- *
  -- *  \return 0 on success, or -1 if the surface is not valid
  -- *
  -- *  You can pass SDL_RLEACCEL to enable RLE accelerated blits.
  --  

   function SDL_SetColorKey
     (surface : access SDL_Surface;
      flag : int;
      key : SDL_stdinc_h.Uint32) return int;  -- ..\SDL2_tmp\SDL_surface.h:248
   pragma Import (C, SDL_SetColorKey, "SDL_SetColorKey");

  --*
  -- *  \brief Returns whether the surface has a color key
  -- *
  -- *  \return SDL_TRUE if the surface has a color key, or SDL_FALSE if the surface is NULL or has no color key
  --  

   function SDL_HasColorKey (surface : access SDL_Surface) return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_surface.h:256
   pragma Import (C, SDL_HasColorKey, "SDL_HasColorKey");

  --*
  -- *  \brief Gets the color key (transparent pixel) in a blittable surface.
  -- *
  -- *  \param surface The surface to update
  -- *  \param key A pointer filled in with the transparent pixel in the native
  -- *             surface format
  -- *
  -- *  \return 0 on success, or -1 if the surface is not valid or colorkey is not
  -- *          enabled.
  --  

   function SDL_GetColorKey (surface : access SDL_Surface; key : access SDL_stdinc_h.Uint32) return int;  -- ..\SDL2_tmp\SDL_surface.h:268
   pragma Import (C, SDL_GetColorKey, "SDL_GetColorKey");

  --*
  -- *  \brief Set an additional color value used in blit operations.
  -- *
  -- *  \param surface The surface to update.
  -- *  \param r The red color value multiplied into blit operations.
  -- *  \param g The green color value multiplied into blit operations.
  -- *  \param b The blue color value multiplied into blit operations.
  -- *
  -- *  \return 0 on success, or -1 if the surface is not valid.
  -- *
  -- *  \sa SDL_GetSurfaceColorMod()
  --  

   function SDL_SetSurfaceColorMod
     (surface : access SDL_Surface;
      r : SDL_stdinc_h.Uint8;
      g : SDL_stdinc_h.Uint8;
      b : SDL_stdinc_h.Uint8) return int;  -- ..\SDL2_tmp\SDL_surface.h:283
   pragma Import (C, SDL_SetSurfaceColorMod, "SDL_SetSurfaceColorMod");

  --*
  -- *  \brief Get the additional color value used in blit operations.
  -- *
  -- *  \param surface The surface to query.
  -- *  \param r A pointer filled in with the current red color value.
  -- *  \param g A pointer filled in with the current green color value.
  -- *  \param b A pointer filled in with the current blue color value.
  -- *
  -- *  \return 0 on success, or -1 if the surface is not valid.
  -- *
  -- *  \sa SDL_SetSurfaceColorMod()
  --  

   function SDL_GetSurfaceColorMod
     (surface : access SDL_Surface;
      r : access SDL_stdinc_h.Uint8;
      g : access SDL_stdinc_h.Uint8;
      b : access SDL_stdinc_h.Uint8) return int;  -- ..\SDL2_tmp\SDL_surface.h:299
   pragma Import (C, SDL_GetSurfaceColorMod, "SDL_GetSurfaceColorMod");

  --*
  -- *  \brief Set an additional alpha value used in blit operations.
  -- *
  -- *  \param surface The surface to update.
  -- *  \param alpha The alpha value multiplied into blit operations.
  -- *
  -- *  \return 0 on success, or -1 if the surface is not valid.
  -- *
  -- *  \sa SDL_GetSurfaceAlphaMod()
  --  

   function SDL_SetSurfaceAlphaMod (surface : access SDL_Surface; alpha : SDL_stdinc_h.Uint8) return int;  -- ..\SDL2_tmp\SDL_surface.h:313
   pragma Import (C, SDL_SetSurfaceAlphaMod, "SDL_SetSurfaceAlphaMod");

  --*
  -- *  \brief Get the additional alpha value used in blit operations.
  -- *
  -- *  \param surface The surface to query.
  -- *  \param alpha A pointer filled in with the current alpha value.
  -- *
  -- *  \return 0 on success, or -1 if the surface is not valid.
  -- *
  -- *  \sa SDL_SetSurfaceAlphaMod()
  --  

   function SDL_GetSurfaceAlphaMod (surface : access SDL_Surface; alpha : access SDL_stdinc_h.Uint8) return int;  -- ..\SDL2_tmp\SDL_surface.h:326
   pragma Import (C, SDL_GetSurfaceAlphaMod, "SDL_GetSurfaceAlphaMod");

  --*
  -- *  \brief Set the blend mode used for blit operations.
  -- *
  -- *  \param surface The surface to update.
  -- *  \param blendMode ::SDL_BlendMode to use for blit blending.
  -- *
  -- *  \return 0 on success, or -1 if the parameters are not valid.
  -- *
  -- *  \sa SDL_GetSurfaceBlendMode()
  --  

   function SDL_SetSurfaceBlendMode (surface : access SDL_Surface; blendMode : SDL_blendmode_h.SDL_BlendMode) return int;  -- ..\SDL2_tmp\SDL_surface.h:339
   pragma Import (C, SDL_SetSurfaceBlendMode, "SDL_SetSurfaceBlendMode");

  --*
  -- *  \brief Get the blend mode used for blit operations.
  -- *
  -- *  \param surface   The surface to query.
  -- *  \param blendMode A pointer filled in with the current blend mode.
  -- *
  -- *  \return 0 on success, or -1 if the surface is not valid.
  -- *
  -- *  \sa SDL_SetSurfaceBlendMode()
  --  

   function SDL_GetSurfaceBlendMode (surface : access SDL_Surface; blendMode : access SDL_blendmode_h.SDL_BlendMode) return int;  -- ..\SDL2_tmp\SDL_surface.h:352
   pragma Import (C, SDL_GetSurfaceBlendMode, "SDL_GetSurfaceBlendMode");

  --*
  -- *  Sets the clipping rectangle for the destination surface in a blit.
  -- *
  -- *  If the clip rectangle is NULL, clipping will be disabled.
  -- *
  -- *  If the clip rectangle doesn't intersect the surface, the function will
  -- *  return SDL_FALSE and blits will be completely clipped.  Otherwise the
  -- *  function returns SDL_TRUE and blits to the surface will be clipped to
  -- *  the intersection of the surface area and the clipping rectangle.
  -- *
  -- *  Note that blits are automatically clipped to the edges of the source
  -- *  and destination surfaces.
  --  

   function SDL_SetClipRect (surface : access SDL_Surface; rect : access constant SDL_rect_h.SDL_Rect) return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_surface.h:368
   pragma Import (C, SDL_SetClipRect, "SDL_SetClipRect");

  --*
  -- *  Gets the clipping rectangle for the destination surface in a blit.
  -- *
  -- *  \c rect must be a pointer to a valid rectangle which will be filled
  -- *  with the correct values.
  --  

   procedure SDL_GetClipRect (surface : access SDL_Surface; rect : access SDL_rect_h.SDL_Rect);  -- ..\SDL2_tmp\SDL_surface.h:377
   pragma Import (C, SDL_GetClipRect, "SDL_GetClipRect");

  -- * Creates a new surface identical to the existing surface
  --  

   function SDL_DuplicateSurface (surface : access SDL_Surface) return access SDL_Surface;  -- ..\SDL2_tmp\SDL_surface.h:383
   pragma Import (C, SDL_DuplicateSurface, "SDL_DuplicateSurface");

  --*
  -- *  Creates a new surface of the specified format, and then copies and maps
  -- *  the given surface to it so the blit of the converted surface will be as
  -- *  fast as possible.  If this function fails, it returns NULL.
  -- *
  -- *  The \c flags parameter is passed to SDL_CreateRGBSurface() and has those
  -- *  semantics.  You can also pass ::SDL_RLEACCEL in the flags parameter and
  -- *  SDL will try to RLE accelerate colorkey and alpha blits in the resulting
  -- *  surface.
  --  

   function SDL_ConvertSurface
     (src : access SDL_Surface;
      fmt : access constant SDL_pixels_h.SDL_PixelFormat;
      flags : SDL_stdinc_h.Uint32) return access SDL_Surface;  -- ..\SDL2_tmp\SDL_surface.h:395
   pragma Import (C, SDL_ConvertSurface, "SDL_ConvertSurface");

   function SDL_ConvertSurfaceFormat
     (src : access SDL_Surface;
      pixel_format : SDL_stdinc_h.Uint32;
      flags : SDL_stdinc_h.Uint32) return access SDL_Surface;  -- ..\SDL2_tmp\SDL_surface.h:397
   pragma Import (C, SDL_ConvertSurfaceFormat, "SDL_ConvertSurfaceFormat");

  --*
  -- * \brief Copy a block of pixels of one format to another format
  -- *
  -- *  \return 0 on success, or -1 if there was an error
  --  

   function SDL_ConvertPixels
     (width : int;
      height : int;
      src_format : SDL_stdinc_h.Uint32;
      src : System.Address;
      src_pitch : int;
      dst_format : SDL_stdinc_h.Uint32;
      dst : System.Address;
      dst_pitch : int) return int;  -- ..\SDL2_tmp\SDL_surface.h:405
   pragma Import (C, SDL_ConvertPixels, "SDL_ConvertPixels");

  --*
  -- *  Performs a fast fill of the given rectangle with \c color.
  -- *
  -- *  If \c rect is NULL, the whole surface will be filled with \c color.
  -- *
  -- *  The color should be a pixel of the format used by the surface, and
  -- *  can be generated by the SDL_MapRGB() function.
  -- *
  -- *  \return 0 on success, or -1 on error.
  --  

   function SDL_FillRect
     (dst : access SDL_Surface;
      rect : access constant SDL_rect_h.SDL_Rect;
      color : SDL_stdinc_h.Uint32) return int;  -- ..\SDL2_tmp\SDL_surface.h:421
   pragma Import (C, SDL_FillRect, "SDL_FillRect");

   function SDL_FillRects
     (dst : access SDL_Surface;
      rects : access constant SDL_rect_h.SDL_Rect;
      count : int;
      color : SDL_stdinc_h.Uint32) return int;  -- ..\SDL2_tmp\SDL_surface.h:423
   pragma Import (C, SDL_FillRects, "SDL_FillRects");

  --*
  -- *  Performs a fast blit from the source surface to the destination surface.
  -- *
  -- *  This assumes that the source and destination rectangles are
  -- *  the same size.  If either \c srcrect or \c dstrect are NULL, the entire
  -- *  surface (\c src or \c dst) is copied.  The final blit rectangles are saved
  -- *  in \c srcrect and \c dstrect after all clipping is performed.
  -- *
  -- *  \return If the blit is successful, it returns 0, otherwise it returns -1.
  -- *
  -- *  The blit function should not be called on a locked surface.
  -- *
  -- *  The blit semantics for surfaces with and without blending and colorkey
  -- *  are defined as follows:
  -- *  \verbatim
  --    RGBA->RGB:
  --      Source surface blend mode set to SDL_BLENDMODE_BLEND:
  --        alpha-blend (using the source alpha-channel and per-surface alpha)
  --        SDL_SRCCOLORKEY ignored.
  --      Source surface blend mode set to SDL_BLENDMODE_NONE:
  --        copy RGB.
  --        if SDL_SRCCOLORKEY set, only copy the pixels matching the
  --        RGB values of the source color key, ignoring alpha in the
  --        comparison.
  --    RGB->RGBA:
  --      Source surface blend mode set to SDL_BLENDMODE_BLEND:
  --        alpha-blend (using the source per-surface alpha)
  --      Source surface blend mode set to SDL_BLENDMODE_NONE:
  --        copy RGB, set destination alpha to source per-surface alpha value.
  --      both:
  --        if SDL_SRCCOLORKEY set, only copy the pixels matching the
  --        source color key.
  --    RGBA->RGBA:
  --      Source surface blend mode set to SDL_BLENDMODE_BLEND:
  --        alpha-blend (using the source alpha-channel and per-surface alpha)
  --        SDL_SRCCOLORKEY ignored.
  --      Source surface blend mode set to SDL_BLENDMODE_NONE:
  --        copy all of RGBA to the destination.
  --        if SDL_SRCCOLORKEY set, only copy the pixels matching the
  --        RGB values of the source color key, ignoring alpha in the
  --        comparison.
  --    RGB->RGB:
  --      Source surface blend mode set to SDL_BLENDMODE_BLEND:
  --        alpha-blend (using the source per-surface alpha)
  --      Source surface blend mode set to SDL_BLENDMODE_NONE:
  --        copy RGB.
  --      both:
  --        if SDL_SRCCOLORKEY set, only copy the pixels matching the
  --        source color key.
  --    \endverbatim
  -- *
  -- *  You should call SDL_BlitSurface() unless you know exactly how SDL
  -- *  blitting works internally and how to use the other blit functions.
  --  

  --*
  -- *  This is the public blit function, SDL_BlitSurface(), and it performs
  -- *  rectangle validation and clipping before passing it to SDL_LowerBlit()
  --  

   function SDL_UpperBlit
     (src : access SDL_Surface;
      srcrect : access constant SDL_rect_h.SDL_Rect;
      dst : access SDL_Surface;
      dstrect : access SDL_rect_h.SDL_Rect) return int;  -- ..\SDL2_tmp\SDL_surface.h:489
   pragma Import (C, SDL_UpperBlit, "SDL_UpperBlit");

  --*
  -- *  This is a semi-private blit function and it performs low-level surface
  -- *  blitting only.
  --  

   function SDL_LowerBlit
     (src : access SDL_Surface;
      srcrect : access SDL_rect_h.SDL_Rect;
      dst : access SDL_Surface;
      dstrect : access SDL_rect_h.SDL_Rect) return int;  -- ..\SDL2_tmp\SDL_surface.h:497
   pragma Import (C, SDL_LowerBlit, "SDL_LowerBlit");

  --*
  -- *  \brief Perform a fast, low quality, stretch blit between two surfaces of the
  -- *         same pixel format.
  -- *
  -- *  \note This function uses a static buffer, and is not thread-safe.
  --  

   function SDL_SoftStretch
     (src : access SDL_Surface;
      srcrect : access constant SDL_rect_h.SDL_Rect;
      dst : access SDL_Surface;
      dstrect : access constant SDL_rect_h.SDL_Rect) return int;  -- ..\SDL2_tmp\SDL_surface.h:507
   pragma Import (C, SDL_SoftStretch, "SDL_SoftStretch");

  --*
  -- *  This is the public scaled blit function, SDL_BlitScaled(), and it performs
  -- *  rectangle validation and clipping before passing it to SDL_LowerBlitScaled()
  --  

   function SDL_UpperBlitScaled
     (src : access SDL_Surface;
      srcrect : access constant SDL_rect_h.SDL_Rect;
      dst : access SDL_Surface;
      dstrect : access SDL_rect_h.SDL_Rect) return int;  -- ..\SDL2_tmp\SDL_surface.h:518
   pragma Import (C, SDL_UpperBlitScaled, "SDL_UpperBlitScaled");

  --*
  -- *  This is a semi-private blit function and it performs low-level surface
  -- *  scaled blitting only.
  --  

   function SDL_LowerBlitScaled
     (src : access SDL_Surface;
      srcrect : access SDL_rect_h.SDL_Rect;
      dst : access SDL_Surface;
      dstrect : access SDL_rect_h.SDL_Rect) return int;  -- ..\SDL2_tmp\SDL_surface.h:526
   pragma Import (C, SDL_LowerBlitScaled, "SDL_LowerBlitScaled");

  --*
  -- *  \brief Set the YUV conversion mode
  --  

   procedure SDL_SetYUVConversionMode (mode : SDL_YUV_CONVERSION_MODE);  -- ..\SDL2_tmp\SDL_surface.h:533
   pragma Import (C, SDL_SetYUVConversionMode, "SDL_SetYUVConversionMode");

  --*
  -- *  \brief Get the YUV conversion mode
  --  

   function SDL_GetYUVConversionMode return SDL_YUV_CONVERSION_MODE;  -- ..\SDL2_tmp\SDL_surface.h:538
   pragma Import (C, SDL_GetYUVConversionMode, "SDL_GetYUVConversionMode");

  --*
  -- *  \brief Get the YUV conversion mode, returning the correct mode for the resolution when the current conversion mode is SDL_YUV_CONVERSION_AUTOMATIC
  --  

   function SDL_GetYUVConversionModeForResolution (width : int; height : int) return SDL_YUV_CONVERSION_MODE;  -- ..\SDL2_tmp\SDL_surface.h:543
   pragma Import (C, SDL_GetYUVConversionModeForResolution, "SDL_GetYUVConversionModeForResolution");

  -- Ends C function definitions when using C++  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_surface_h;
