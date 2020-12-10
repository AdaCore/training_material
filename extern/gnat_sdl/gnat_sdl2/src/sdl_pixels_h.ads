pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_stdinc_h;
with Interfaces.C.Strings;

package SDL_pixels_h is

   SDL_ALPHA_OPAQUE : constant := 255;  --  ..\SDL2_tmp\SDL_pixels.h:46
   SDL_ALPHA_TRANSPARENT : constant := 0;  --  ..\SDL2_tmp\SDL_pixels.h:47
 
   -- Manual Fix for unnamed enum   
   --   /** Pixel type. */
   --enum
   --{
   type SDL_PIXELTYPE_TYPE is (SDL_PIXELTYPE_UNKNOWN,
                           SDL_PIXELTYPE_INDEX1,
                           SDL_PIXELTYPE_INDEX4,
                           SDL_PIXELTYPE_INDEX8,
                           SDL_PIXELTYPE_PACKED8,
                           SDL_PIXELTYPE_PACKED16,
                           SDL_PIXELTYPE_PACKED32,
                           SDL_PIXELTYPE_ARRAYU8,
                           SDL_PIXELTYPE_ARRAYU16,
                           SDL_PIXELTYPE_ARRAYU32,
                           SDL_PIXELTYPE_ARRAYF16,
                           SDL_PIXELTYPE_ARRAYF32);
--};

--/** Bitmap pixel order, high bit -> low bit. */
--enum
--{
   type SDL_BITMAPORDER_TYPE is (
                                 SDL_BITMAPORDER_NONE,
                                 SDL_BITMAPORDER_4321,
                                 SDL_BITMAPORDER_1234);
--};

--/** Packed component order, high bit -> low bit. */
--enum
--{
   type SDL_PACKEDORDER_TYPE is (
    SDL_PACKEDORDER_NONE,
    SDL_PACKEDORDER_XRGB,
    SDL_PACKEDORDER_RGBX,
    SDL_PACKEDORDER_ARGB,
    SDL_PACKEDORDER_RGBA,
    SDL_PACKEDORDER_XBGR,
    SDL_PACKEDORDER_BGRX,
    SDL_PACKEDORDER_ABGR,
    SDL_PACKEDORDER_BGRA);
--};

--   /** Array component order, low byte -> high byte. */
--/* !!! FIXME: in 2.1, make these not overlap differently with
--   !!! FIXME:  SDL_PACKEDORDER_*, so we can simplify SDL_ISPIXELFORMAT_ALPHA */
--enum
--{
   type SDL_ARRAYORDER_TYPE is (
                                SDL_ARRAYORDER_NONE,
                                SDL_ARRAYORDER_RGB,
                                SDL_ARRAYORDER_RGBA,
                                SDL_ARRAYORDER_ARGB,
                                SDL_ARRAYORDER_BGR,
                                SDL_ARRAYORDER_BGRA,
                                SDL_ARRAYORDER_ABGR);
--};

--/** Packed component layout. */
--enum
--{
   type SDL_PACKEDLAYOUT_TYPE is (
    SDL_PACKEDLAYOUT_NONE,
    SDL_PACKEDLAYOUT_332,
    SDL_PACKEDLAYOUT_4444,
    SDL_PACKEDLAYOUT_1555,
    SDL_PACKEDLAYOUT_5551,
    SDL_PACKEDLAYOUT_565,
    SDL_PACKEDLAYOUT_8888,
    SDL_PACKEDLAYOUT_2101010,
    SDL_PACKEDLAYOUT_1010102);
--};

   
   
   
   --  arg-macro: procedure SDL_DEFINE_PIXELFOURCC (A, B, C, D)
   --    SDL_FOURCC(A, B, C, D)
   --  arg-macro: function SDL_DEFINE_PIXELFORMAT (type, order, layout, bits, bytes)
   --    return (2 ** 28) or ((type) << 24) or ((order) << 20) or ((layout) << 16) or ((bits) << 8) or ((bytes) << 0);
   --  arg-macro: function SDL_PIXELFLAG (X)
   --    return ((X) >> 28) and 16#0F#;
   --  arg-macro: function SDL_PIXELTYPE (X)
   --    return ((X) >> 24) and 16#0F#;
   --  arg-macro: function SDL_PIXELORDER (X)
   --    return ((X) >> 20) and 16#0F#;
   --  arg-macro: function SDL_PIXELLAYOUT (X)
   --    return ((X) >> 16) and 16#0F#;
   --  arg-macro: function SDL_BITSPERPIXEL (X)
   --    return ((X) >> 8) and 16#FF#;
   --  arg-macro: function SDL_BYTESPERPIXEL (X)
   --    return SDL_ISPIXELFORMAT_FOURCC(X) ? ((((X) = SDL_PIXELFORMAT_YUY2)  or else  ((X) = SDL_PIXELFORMAT_UYVY)  or else  ((X) = SDL_PIXELFORMAT_YVYU)) ? 2 : 1) : (((X) >> 0) and 16#FF#);
   --  arg-macro: function SDL_ISPIXELFORMAT_INDEXED (format)
   --    return notSDL_ISPIXELFORMAT_FOURCC(format)  and then  ((SDL_PIXELTYPE(format) = SDL_PIXELTYPE_INDEX1)  or else  (SDL_PIXELTYPE(format) = SDL_PIXELTYPE_INDEX4)  or else  (SDL_PIXELTYPE(format) = SDL_PIXELTYPE_INDEX8));
   --  arg-macro: function SDL_ISPIXELFORMAT_PACKED (format)
   --    return notSDL_ISPIXELFORMAT_FOURCC(format)  and then  ((SDL_PIXELTYPE(format) = SDL_PIXELTYPE_PACKED8)  or else  (SDL_PIXELTYPE(format) = SDL_PIXELTYPE_PACKED16)  or else  (SDL_PIXELTYPE(format) = SDL_PIXELTYPE_PACKED32));
   --  arg-macro: function SDL_ISPIXELFORMAT_ARRAY (format)
   --    return notSDL_ISPIXELFORMAT_FOURCC(format)  and then  ((SDL_PIXELTYPE(format) = SDL_PIXELTYPE_ARRAYU8)  or else  (SDL_PIXELTYPE(format) = SDL_PIXELTYPE_ARRAYU16)  or else  (SDL_PIXELTYPE(format) = SDL_PIXELTYPE_ARRAYU32)  or else  (SDL_PIXELTYPE(format) = SDL_PIXELTYPE_ARRAYF16)  or else  (SDL_PIXELTYPE(format) = SDL_PIXELTYPE_ARRAYF32));
   --  arg-macro: function SDL_ISPIXELFORMAT_ALPHA (format)
   --    return (SDL_ISPIXELFORMAT_PACKED(format)  and then  ((SDL_PIXELORDER(format) = SDL_PACKEDORDER_ARGB)  or else  (SDL_PIXELORDER(format) = SDL_PACKEDORDER_RGBA)  or else  (SDL_PIXELORDER(format) = SDL_PACKEDORDER_ABGR)  or else  (SDL_PIXELORDER(format) = SDL_PACKEDORDER_BGRA)))  or else  (SDL_ISPIXELFORMAT_ARRAY(format)  and then  ((SDL_PIXELORDER(format) = SDL_ARRAYORDER_ARGB)  or else  (SDL_PIXELORDER(format) = SDL_ARRAYORDER_RGBA)  or else  (SDL_PIXELORDER(format) = SDL_ARRAYORDER_ABGR)  or else  (SDL_PIXELORDER(format) = SDL_ARRAYORDER_BGRA)));
   --  arg-macro: function SDL_ISPIXELFORMAT_FOURCC (format)
   --    return (format)  and then  (SDL_PIXELFLAG(format) /= 1);
   --  unsupported macro: SDL_Colour SDL_Color

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
  -- *  \file SDL_pixels.h
  -- *
  -- *  Header for the enumerated pixel format definitions.
  --  

  -- Set up for C function definitions, even when using C++  
  --*
  -- *  \name Transparency definitions
  -- *
  -- *  These define alpha as the opacity of a surface.
  --  

  -- @{  
  -- @}  
  --* Pixel type.  
  --* Bitmap pixel order, high bit -> low bit.  
  --* Packed component order, high bit -> low bit.  
  --* Array component order, low byte -> high byte.  
  -- !!! FIXME: in 2.1, make these not overlap differently with
  --   !!! FIXME:  SDL_PACKEDORDER_*, so we can simplify SDL_ISPIXELFORMAT_ALPHA  

  --* Packed component layout.  
  -- The flag is set to 1 because 0x1? is not in the printable ASCII range  
  -- Note: If you modify this list, update SDL_GetPixelFormatName()  
  -- Aliases for RGBA byte arrays of color data, for the current platform  
  --*< Planar mode: Y + V + U  (3 planes)  
  --*< Planar mode: Y + U + V  (3 planes)  
  --*< Packed mode: Y0+U0+Y1+V0 (1 plane)  
  --*< Packed mode: U0+Y0+V0+Y1 (1 plane)  
  --*< Packed mode: Y0+V0+Y1+U0 (1 plane)  
  --*< Planar mode: Y + U/V interleaved  (2 planes)  
  --*< Planar mode: Y + V/U interleaved  (2 planes)  
  --*< Android video texture format  
   type SDL_Color is record
      r : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_pixels.h:297
      g : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_pixels.h:298
      b : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_pixels.h:299
      a : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_pixels.h:300
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_Color);  -- ..\SDL2_tmp\SDL_pixels.h:295

   type SDL_Palette is record
      ncolors : aliased int;  -- ..\SDL2_tmp\SDL_pixels.h:306
      colors : access SDL_Color;  -- ..\SDL2_tmp\SDL_pixels.h:307
      version : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_pixels.h:308
      refcount : aliased int;  -- ..\SDL2_tmp\SDL_pixels.h:309
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_Palette);  -- ..\SDL2_tmp\SDL_pixels.h:304

  --*
  -- *  \note Everything in the pixel format structure is read-only.
  --  

   type SDL_PixelFormat_padding_array is array (0 .. 1) of aliased SDL_stdinc_h.Uint8;
   type SDL_PixelFormat;
   type SDL_PixelFormat is record
      format : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_pixels.h:317
      palette : access SDL_Palette;  -- ..\SDL2_tmp\SDL_pixels.h:318
      BitsPerPixel : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_pixels.h:319
      BytesPerPixel : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_pixels.h:320
      padding : aliased SDL_PixelFormat_padding_array;  -- ..\SDL2_tmp\SDL_pixels.h:321
      Rmask : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_pixels.h:322
      Gmask : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_pixels.h:323
      Bmask : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_pixels.h:324
      Amask : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_pixels.h:325
      Rloss : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_pixels.h:326
      Gloss : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_pixels.h:327
      Bloss : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_pixels.h:328
      Aloss : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_pixels.h:329
      Rshift : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_pixels.h:330
      Gshift : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_pixels.h:331
      Bshift : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_pixels.h:332
      Ashift : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_pixels.h:333
      refcount : aliased int;  -- ..\SDL2_tmp\SDL_pixels.h:334
      next : access SDL_PixelFormat;  -- ..\SDL2_tmp\SDL_pixels.h:335
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_PixelFormat);  -- ..\SDL2_tmp\SDL_pixels.h:315

  --*
  -- * \brief Get the human readable name of a pixel format
  --  

   function SDL_GetPixelFormatName (format : SDL_stdinc_h.Uint32) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_pixels.h:341
   pragma Import (C, SDL_GetPixelFormatName, "SDL_GetPixelFormatName");

  --*
  -- *  \brief Convert one of the enumerated pixel formats to a bpp and RGBA masks.
  -- *
  -- *  \return SDL_TRUE, or SDL_FALSE if the conversion wasn't possible.
  -- *
  -- *  \sa SDL_MasksToPixelFormatEnum()
  --  

   function SDL_PixelFormatEnumToMasks
     (format : SDL_stdinc_h.Uint32;
      bpp : access int;
      Rmask : access SDL_stdinc_h.Uint32;
      Gmask : access SDL_stdinc_h.Uint32;
      Bmask : access SDL_stdinc_h.Uint32;
      Amask : access SDL_stdinc_h.Uint32) return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_pixels.h:350
   pragma Import (C, SDL_PixelFormatEnumToMasks, "SDL_PixelFormatEnumToMasks");

  --*
  -- *  \brief Convert a bpp and RGBA masks to an enumerated pixel format.
  -- *
  -- *  \return The pixel format, or ::SDL_PIXELFORMAT_UNKNOWN if the conversion
  -- *          wasn't possible.
  -- *
  -- *  \sa SDL_PixelFormatEnumToMasks()
  --  

   function SDL_MasksToPixelFormatEnum
     (bpp : int;
      Rmask : SDL_stdinc_h.Uint32;
      Gmask : SDL_stdinc_h.Uint32;
      Bmask : SDL_stdinc_h.Uint32;
      Amask : SDL_stdinc_h.Uint32) return SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_pixels.h:365
   pragma Import (C, SDL_MasksToPixelFormatEnum, "SDL_MasksToPixelFormatEnum");

  --*
  -- *  \brief Create an SDL_PixelFormat structure from a pixel format enum.
  --  

   function SDL_AllocFormat (pixel_format : SDL_stdinc_h.Uint32) return access SDL_PixelFormat;  -- ..\SDL2_tmp\SDL_pixels.h:374
   pragma Import (C, SDL_AllocFormat, "SDL_AllocFormat");

  --*
  -- *  \brief Free an SDL_PixelFormat structure.
  --  

   procedure SDL_FreeFormat (format : access SDL_PixelFormat);  -- ..\SDL2_tmp\SDL_pixels.h:379
   pragma Import (C, SDL_FreeFormat, "SDL_FreeFormat");

  --*
  -- *  \brief Create a palette structure with the specified number of color
  -- *         entries.
  -- *
  -- *  \return A new palette, or NULL if there wasn't enough memory.
  -- *
  -- *  \note The palette entries are initialized to white.
  -- *
  -- *  \sa SDL_FreePalette()
  --  

   function SDL_AllocPalette (ncolors : int) return access SDL_Palette;  -- ..\SDL2_tmp\SDL_pixels.h:391
   pragma Import (C, SDL_AllocPalette, "SDL_AllocPalette");

  --*
  -- *  \brief Set the palette for a pixel format structure.
  --  

   function SDL_SetPixelFormatPalette (format : access SDL_PixelFormat; palette : access SDL_Palette) return int;  -- ..\SDL2_tmp\SDL_pixels.h:396
   pragma Import (C, SDL_SetPixelFormatPalette, "SDL_SetPixelFormatPalette");

  --*
  -- *  \brief Set a range of colors in a palette.
  -- *
  -- *  \param palette    The palette to modify.
  -- *  \param colors     An array of colors to copy into the palette.
  -- *  \param firstcolor The index of the first palette entry to modify.
  -- *  \param ncolors    The number of entries to modify.
  -- *
  -- *  \return 0 on success, or -1 if not all of the colors could be set.
  --  

   function SDL_SetPaletteColors
     (palette : access SDL_Palette;
      colors : access constant SDL_Color;
      firstcolor : int;
      ncolors : int) return int;  -- ..\SDL2_tmp\SDL_pixels.h:409
   pragma Import (C, SDL_SetPaletteColors, "SDL_SetPaletteColors");

  --*
  -- *  \brief Free a palette created with SDL_AllocPalette().
  -- *
  -- *  \sa SDL_AllocPalette()
  --  

   procedure SDL_FreePalette (palette : access SDL_Palette);  -- ..\SDL2_tmp\SDL_pixels.h:418
   pragma Import (C, SDL_FreePalette, "SDL_FreePalette");

  --*
  -- *  \brief Maps an RGB triple to an opaque pixel value for a given pixel format.
  -- *
  -- *  \sa SDL_MapRGBA
  --  

   function SDL_MapRGB
     (format : access constant SDL_PixelFormat;
      r : SDL_stdinc_h.Uint8;
      g : SDL_stdinc_h.Uint8;
      b : SDL_stdinc_h.Uint8) return SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_pixels.h:425
   pragma Import (C, SDL_MapRGB, "SDL_MapRGB");

  --*
  -- *  \brief Maps an RGBA quadruple to a pixel value for a given pixel format.
  -- *
  -- *  \sa SDL_MapRGB
  --  

   function SDL_MapRGBA

     (format : access constant SDL_PixelFormat;
      r : SDL_stdinc_h.Uint8;
      g : SDL_stdinc_h.Uint8;
      b : SDL_stdinc_h.Uint8;
      a : SDL_stdinc_h.Uint8) return SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_pixels.h:433
   pragma Import (C, SDL_MapRGBA, "SDL_MapRGBA");

  --*
  -- *  \brief Get the RGB components from a pixel of the specified format.
  -- *
  -- *  \sa SDL_GetRGBA
  --  

   procedure SDL_GetRGB
     (pixel : SDL_stdinc_h.Uint32;
      format : access constant SDL_PixelFormat;
      r : access SDL_stdinc_h.Uint8;
      g : access SDL_stdinc_h.Uint8;
      b : access SDL_stdinc_h.Uint8);  -- ..\SDL2_tmp\SDL_pixels.h:442
   pragma Import (C, SDL_GetRGB, "SDL_GetRGB");

  --*
  -- *  \brief Get the RGBA components from a pixel of the specified format.
  -- *
  -- *  \sa SDL_GetRGB
  --  

   procedure SDL_GetRGBA
     (pixel : SDL_stdinc_h.Uint32;
      format : access constant SDL_PixelFormat;
      r : access SDL_stdinc_h.Uint8;
      g : access SDL_stdinc_h.Uint8;
      b : access SDL_stdinc_h.Uint8;
      a : access SDL_stdinc_h.Uint8);  -- ..\SDL2_tmp\SDL_pixels.h:451
   pragma Import (C, SDL_GetRGBA, "SDL_GetRGBA");

  --*
  -- *  \brief Calculate a 256 entry gamma ramp for a gamma value.
  --  

   procedure SDL_CalculateGammaRamp (gamma : float; ramp : access SDL_stdinc_h.Uint16);  -- ..\SDL2_tmp\SDL_pixels.h:459
   pragma Import (C, SDL_CalculateGammaRamp, "SDL_CalculateGammaRamp");

  -- Ends C function definitions when using C++  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_pixels_h;
