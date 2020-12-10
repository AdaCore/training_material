pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_SDL_stdinc_h;
with System;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
limited with SDL_SDL_rwops_h;

package SDL_SDL_video_h is


   SDL_ALPHA_OPAQUE : constant := 255;  --  ../include/SDL/SDL_video.h:44
   SDL_ALPHA_TRANSPARENT : constant := 0;  --  ../include/SDL/SDL_video.h:45
   --  unsupported macro: SDL_Colour SDL_Color

   SDL_SWSURFACE : constant := 16#00000000#;  --  ../include/SDL/SDL_video.h:131
   SDL_HWSURFACE : constant := 16#00000001#;  --  ../include/SDL/SDL_video.h:132
   SDL_ASYNCBLIT : constant := 16#00000004#;  --  ../include/SDL/SDL_video.h:133

   SDL_ANYFORMAT : constant := 16#10000000#;  --  ../include/SDL/SDL_video.h:138
   SDL_HWPALETTE : constant := 16#20000000#;  --  ../include/SDL/SDL_video.h:139
   SDL_DOUBLEBUF : constant := 16#40000000#;  --  ../include/SDL/SDL_video.h:140
   SDL_FULLSCREEN : constant := 16#80000000#;  --  ../include/SDL/SDL_video.h:141
   SDL_OPENGL : constant := 16#00000002#;  --  ../include/SDL/SDL_video.h:142
   SDL_OPENGLBLIT : constant := 16#0000000A#;  --  ../include/SDL/SDL_video.h:143
   SDL_RESIZABLE : constant := 16#00000010#;  --  ../include/SDL/SDL_video.h:144
   SDL_NOFRAME : constant := 16#00000020#;  --  ../include/SDL/SDL_video.h:145

   SDL_HWACCEL : constant := 16#00000100#;  --  ../include/SDL/SDL_video.h:150
   SDL_SRCCOLORKEY : constant := 16#00001000#;  --  ../include/SDL/SDL_video.h:151
   SDL_RLEACCELOK : constant := 16#00002000#;  --  ../include/SDL/SDL_video.h:152
   SDL_RLEACCEL : constant := 16#00004000#;  --  ../include/SDL/SDL_video.h:153
   SDL_SRCALPHA : constant := 16#00010000#;  --  ../include/SDL/SDL_video.h:154
   SDL_PREALLOC : constant := 16#01000000#;  --  ../include/SDL/SDL_video.h:155
   --  arg-macro: function SDL_MUSTLOCK (surface)
   --    return surface.offset  or else  ((surface.flags and (SDL_HWSURFACEorSDL_ASYNCBLITorSDL_RLEACCEL)) /= 0);

   SDL_YV12_OVERLAY : constant := 16#32315659#;  --  ../include/SDL/SDL_video.h:200
   SDL_IYUV_OVERLAY : constant := 16#56555949#;  --  ../include/SDL/SDL_video.h:201
   SDL_YUY2_OVERLAY : constant := 16#32595559#;  --  ../include/SDL/SDL_video.h:202
   SDL_UYVY_OVERLAY : constant := 16#59565955#;  --  ../include/SDL/SDL_video.h:203
   SDL_YVYU_OVERLAY : constant := 16#55595659#;  --  ../include/SDL/SDL_video.h:204

   SDL_LOGPAL : constant := 16#01#;  --  ../include/SDL/SDL_video.h:252
   SDL_PHYSPAL : constant := 16#02#;  --  ../include/SDL/SDL_video.h:253
   --  unsupported macro: SDL_AllocSurface SDL_CreateRGBSurface
   --  arg-macro: procedure SDL_LoadBMP (file)
   --    SDL_LoadBMP_RW(SDL_RWFromFile(file, "rb"), 1)
   --  arg-macro: procedure SDL_SaveBMP (surface, file)
   --    SDL_SaveBMP_RW(surface, SDL_RWFromFile(file, "wb"), 1)
   --  unsupported macro: SDL_BlitSurface SDL_UpperBlit

   type SDL_Rect is record
      x : aliased SDL_SDL_stdinc_h.Sint16;  -- ../include/SDL/SDL_video.h:51
      y : aliased SDL_SDL_stdinc_h.Sint16;  -- ../include/SDL/SDL_video.h:51
      w : aliased SDL_SDL_stdinc_h.Uint16;  -- ../include/SDL/SDL_video.h:52
      h : aliased SDL_SDL_stdinc_h.Uint16;  -- ../include/SDL/SDL_video.h:52
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_Rect);  -- ../include/SDL/SDL_video.h:50

   type SDL_Color is record
      r : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_video.h:56
      g : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_video.h:57
      b : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_video.h:58
      unused : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_video.h:59
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_Color);  -- ../include/SDL/SDL_video.h:55

   type SDL_Palette is record
      ncolors : aliased int;  -- ../include/SDL/SDL_video.h:64
      colors : access SDL_Color;  -- ../include/SDL/SDL_video.h:65
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_Palette);  -- ../include/SDL/SDL_video.h:63

   type SDL_PixelFormat is record
      palette : access SDL_Palette;  -- ../include/SDL/SDL_video.h:71
      BitsPerPixel : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_video.h:72
      BytesPerPixel : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_video.h:73
      Rloss : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_video.h:74
      Gloss : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_video.h:75
      Bloss : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_video.h:76
      Aloss : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_video.h:77
      Rshift : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_video.h:78
      Gshift : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_video.h:79
      Bshift : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_video.h:80
      Ashift : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_video.h:81
      Rmask : aliased SDL_SDL_stdinc_h.Uint32;  -- ../include/SDL/SDL_video.h:82
      Gmask : aliased SDL_SDL_stdinc_h.Uint32;  -- ../include/SDL/SDL_video.h:83
      Bmask : aliased SDL_SDL_stdinc_h.Uint32;  -- ../include/SDL/SDL_video.h:84
      Amask : aliased SDL_SDL_stdinc_h.Uint32;  -- ../include/SDL/SDL_video.h:85
      colorkey : aliased SDL_SDL_stdinc_h.Uint32;  -- ../include/SDL/SDL_video.h:88
      alpha : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_video.h:90
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_PixelFormat);  -- ../include/SDL/SDL_video.h:70

   type SDL_Surface is record
      flags : aliased SDL_SDL_stdinc_h.Uint32;  -- ../include/SDL/SDL_video.h:97
      format : access SDL_PixelFormat;  -- ../include/SDL/SDL_video.h:98
      w : aliased int;  -- ../include/SDL/SDL_video.h:99
      h : aliased int;  -- ../include/SDL/SDL_video.h:99
      pitch : aliased SDL_SDL_stdinc_h.Uint16;  -- ../include/SDL/SDL_video.h:100
      pixels : System.Address;  -- ../include/SDL/SDL_video.h:101
      offset : aliased int;  -- ../include/SDL/SDL_video.h:102
      hwdata : System.Address;  -- ../include/SDL/SDL_video.h:105
      clip_rect : aliased SDL_Rect;  -- ../include/SDL/SDL_video.h:108
      unused1 : aliased SDL_SDL_stdinc_h.Uint32;  -- ../include/SDL/SDL_video.h:109
      locked : aliased SDL_SDL_stdinc_h.Uint32;  -- ../include/SDL/SDL_video.h:112
      map : System.Address;  -- ../include/SDL/SDL_video.h:115
      format_version : aliased unsigned;  -- ../include/SDL/SDL_video.h:118
      refcount : aliased int;  -- ../include/SDL/SDL_video.h:121
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_Surface);  -- ../include/SDL/SDL_video.h:96

   --  skipped empty struct private_hwdata

   --  skipped empty struct SDL_BlitMap

   type SDL_blit is access function
        (arg1 : access SDL_Surface;
         arg2 : access SDL_Rect;
         arg3 : access SDL_Surface;
         arg4 : access SDL_Rect) return int;
   pragma Convention (C, SDL_blit);  -- ../include/SDL/SDL_video.h:166

   type SDL_VideoInfo is record
      hw_available : Extensions.Unsigned_1;  -- ../include/SDL/SDL_video.h:172
      wm_available : Extensions.Unsigned_1;  -- ../include/SDL/SDL_video.h:173
      UnusedBits1 : Extensions.Unsigned_6;  -- ../include/SDL/SDL_video.h:174
      UnusedBits2 : Extensions.Unsigned_1;  -- ../include/SDL/SDL_video.h:175
      blit_hw : Extensions.Unsigned_1;  -- ../include/SDL/SDL_video.h:176
      blit_hw_CC : Extensions.Unsigned_1;  -- ../include/SDL/SDL_video.h:177
      blit_hw_A : Extensions.Unsigned_1;  -- ../include/SDL/SDL_video.h:178
      blit_sw : Extensions.Unsigned_1;  -- ../include/SDL/SDL_video.h:179
      blit_sw_CC : Extensions.Unsigned_1;  -- ../include/SDL/SDL_video.h:180
      blit_sw_A : Extensions.Unsigned_1;  -- ../include/SDL/SDL_video.h:181
      blit_fill : Extensions.Unsigned_1;  -- ../include/SDL/SDL_video.h:182
      UnusedBits3 : aliased unsigned_short;  -- ../include/SDL/SDL_video.h:183
      video_mem : aliased SDL_SDL_stdinc_h.Uint32;  -- ../include/SDL/SDL_video.h:184
      vfmt : access SDL_PixelFormat;  -- ../include/SDL/SDL_video.h:185
      current_w : aliased int;  -- ../include/SDL/SDL_video.h:186
      current_h : aliased int;  -- ../include/SDL/SDL_video.h:187
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_VideoInfo);
   pragma Pack (SDL_VideoInfo);  -- ../include/SDL/SDL_video.h:171

   type SDL_Overlay is record
      format : aliased SDL_SDL_stdinc_h.Uint32;  -- ../include/SDL/SDL_video.h:209
      w : aliased int;  -- ../include/SDL/SDL_video.h:210
      h : aliased int;  -- ../include/SDL/SDL_video.h:210
      planes : aliased int;  -- ../include/SDL/SDL_video.h:211
      pitches : access SDL_SDL_stdinc_h.Uint16;  -- ../include/SDL/SDL_video.h:212
      pixels : System.Address;  -- ../include/SDL/SDL_video.h:213
      hwfuncs : System.Address;  -- ../include/SDL/SDL_video.h:217
      hwdata : System.Address;  -- ../include/SDL/SDL_video.h:218
      hw_overlay : Extensions.Unsigned_1;  -- ../include/SDL/SDL_video.h:223
      UnusedBits : Extensions.Unsigned_31;  -- ../include/SDL/SDL_video.h:224
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_Overlay);
   pragma Pack (SDL_Overlay);  -- ../include/SDL/SDL_video.h:208

   --  skipped empty struct private_yuvhwfuncs

   --  skipped empty struct private_yuvhwdata

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
      SDL_GL_SWAP_CONTROL);
   pragma Convention (C, SDL_GLattr);  -- ../include/SDL/SDL_video.h:248

   function SDL_VideoInit (driver_name : Interfaces.C.Strings.chars_ptr; flags : SDL_SDL_stdinc_h.Uint32) return int;  -- ../include/SDL/SDL_video.h:275
   pragma Import (C, SDL_VideoInit, "SDL_VideoInit");

   procedure SDL_VideoQuit;  -- ../include/SDL/SDL_video.h:276
   pragma Import (C, SDL_VideoQuit, "SDL_VideoQuit");

   function SDL_VideoDriverName (namebuf : Interfaces.C.Strings.chars_ptr; maxlen : int) return Interfaces.C.Strings.chars_ptr;  -- ../include/SDL/SDL_video.h:284
   pragma Import (C, SDL_VideoDriverName, "SDL_VideoDriverName");

   function SDL_GetVideoSurface return access SDL_Surface;  -- ../include/SDL/SDL_video.h:292
   pragma Import (C, SDL_GetVideoSurface, "SDL_GetVideoSurface");

   function SDL_GetVideoInfo return System.Address;  -- ../include/SDL/SDL_video.h:300
   pragma Import (C, SDL_GetVideoInfo, "SDL_GetVideoInfo");

   function SDL_VideoModeOK
     (width : int;
      height : int;
      bpp : int;
      flags : SDL_SDL_stdinc_h.Uint32) return int;  -- ../include/SDL/SDL_video.h:313
   pragma Import (C, SDL_VideoModeOK, "SDL_VideoModeOK");

   function SDL_ListModes (format : access SDL_PixelFormat; flags : SDL_SDL_stdinc_h.Uint32) return System.Address;  -- ../include/SDL/SDL_video.h:324
   pragma Import (C, SDL_ListModes, "SDL_ListModes");

   function SDL_SetVideoMode
     (width : int;
      height : int;
      bpp : int;
      flags : SDL_SDL_stdinc_h.Uint32) return access SDL_Surface;  -- ../include/SDL/SDL_video.h:384
   pragma Import (C, SDL_SetVideoMode, "SDL_SetVideoMode");

   procedure SDL_UpdateRects
     (screen : access SDL_Surface;
      numrects : int;
      rects : access SDL_Rect);  -- ../include/SDL/SDL_video.h:394
   pragma Import (C, SDL_UpdateRects, "SDL_UpdateRects");

   procedure SDL_UpdateRect
     (screen : access SDL_Surface;
      x : SDL_SDL_stdinc_h.Sint32;
      y : SDL_SDL_stdinc_h.Sint32;
      w : SDL_SDL_stdinc_h.Uint32;
      h : SDL_SDL_stdinc_h.Uint32);  -- ../include/SDL/SDL_video.h:400
   pragma Import (C, SDL_UpdateRect, "SDL_UpdateRect");

   function SDL_Flip (screen : access SDL_Surface) return int;  -- ../include/SDL/SDL_video.h:414
   pragma Import (C, SDL_Flip, "SDL_Flip");

   function SDL_SetGamma
     (red : float;
      green : float;
      blue : float) return int;  -- ../include/SDL/SDL_video.h:424
   pragma Import (C, SDL_SetGamma, "SDL_SetGamma");

   function SDL_SetGammaRamp
     (red : access SDL_SDL_stdinc_h.Uint16;
      green : access SDL_SDL_stdinc_h.Uint16;
      blue : access SDL_SDL_stdinc_h.Uint16) return int;  -- ../include/SDL/SDL_video.h:438
   pragma Import (C, SDL_SetGammaRamp, "SDL_SetGammaRamp");

   function SDL_GetGammaRamp
     (red : access SDL_SDL_stdinc_h.Uint16;
      green : access SDL_SDL_stdinc_h.Uint16;
      blue : access SDL_SDL_stdinc_h.Uint16) return int;  -- ../include/SDL/SDL_video.h:449
   pragma Import (C, SDL_GetGammaRamp, "SDL_GetGammaRamp");

   function SDL_SetColors
     (surface : access SDL_Surface;
      colors : access SDL_Color;
      firstcolor : int;
      ncolors : int) return int;  -- ../include/SDL/SDL_video.h:466
   pragma Import (C, SDL_SetColors, "SDL_SetColors");

   function SDL_SetPalette
     (surface : access SDL_Surface;
      flags : int;
      colors : access SDL_Color;
      firstcolor : int;
      ncolors : int) return int;  -- ../include/SDL/SDL_video.h:485
   pragma Import (C, SDL_SetPalette, "SDL_SetPalette");

   function SDL_MapRGB
     (format : System.Address;
      r : SDL_SDL_stdinc_h.Uint8;
      g : SDL_SDL_stdinc_h.Uint8;
      b : SDL_SDL_stdinc_h.Uint8) return SDL_SDL_stdinc_h.Uint32;  -- ../include/SDL/SDL_video.h:492
   pragma Import (C, SDL_MapRGB, "SDL_MapRGB");

   function SDL_MapRGBA
     (format : System.Address;
      r : SDL_SDL_stdinc_h.Uint8;
      g : SDL_SDL_stdinc_h.Uint8;
      b : SDL_SDL_stdinc_h.Uint8;
      a : SDL_SDL_stdinc_h.Uint8) return SDL_SDL_stdinc_h.Uint32;  -- ../include/SDL/SDL_video.h:499
   pragma Import (C, SDL_MapRGBA, "SDL_MapRGBA");

   procedure SDL_GetRGB
     (pixel : SDL_SDL_stdinc_h.Uint32;
      fmt : System.Address;
      r : access SDL_SDL_stdinc_h.Uint8;
      g : access SDL_SDL_stdinc_h.Uint8;
      b : access SDL_SDL_stdinc_h.Uint8);  -- ../include/SDL/SDL_video.h:506
   pragma Import (C, SDL_GetRGB, "SDL_GetRGB");

   procedure SDL_GetRGBA
     (pixel : SDL_SDL_stdinc_h.Uint32;
      fmt : System.Address;
      r : access SDL_SDL_stdinc_h.Uint8;
      g : access SDL_SDL_stdinc_h.Uint8;
      b : access SDL_SDL_stdinc_h.Uint8;
      a : access SDL_SDL_stdinc_h.Uint8);  -- ../include/SDL/SDL_video.h:513
   pragma Import (C, SDL_GetRGBA, "SDL_GetRGBA");

   function SDL_CreateRGBSurface
     (flags : SDL_SDL_stdinc_h.Uint32;
      width : int;
      height : int;
      depth : int;
      Rmask : SDL_SDL_stdinc_h.Uint32;
      Gmask : SDL_SDL_stdinc_h.Uint32;
      Bmask : SDL_SDL_stdinc_h.Uint32;
      Amask : SDL_SDL_stdinc_h.Uint32) return access SDL_Surface;  -- ../include/SDL/SDL_video.h:553
   pragma Import (C, SDL_CreateRGBSurface, "SDL_CreateRGBSurface");

   function SDL_CreateRGBSurfaceFrom
     (pixels : System.Address;
      width : int;
      height : int;
      depth : int;
      pitch : int;
      Rmask : SDL_SDL_stdinc_h.Uint32;
      Gmask : SDL_SDL_stdinc_h.Uint32;
      Bmask : SDL_SDL_stdinc_h.Uint32;
      Amask : SDL_SDL_stdinc_h.Uint32) return access SDL_Surface;  -- ../include/SDL/SDL_video.h:557
   pragma Import (C, SDL_CreateRGBSurfaceFrom, "SDL_CreateRGBSurfaceFrom");

   procedure SDL_FreeSurface (surface : access SDL_Surface);  -- ../include/SDL/SDL_video.h:560
   pragma Import (C, SDL_FreeSurface, "SDL_FreeSurface");

   function SDL_LockSurface (surface : access SDL_Surface) return int;  -- ../include/SDL/SDL_video.h:580
   pragma Import (C, SDL_LockSurface, "SDL_LockSurface");

   procedure SDL_UnlockSurface (surface : access SDL_Surface);  -- ../include/SDL/SDL_video.h:581
   pragma Import (C, SDL_UnlockSurface, "SDL_UnlockSurface");

   function SDL_LoadBMP_RW (src : access SDL_SDL_rwops_h.SDL_RWops; freesrc : int) return access SDL_Surface;  -- ../include/SDL/SDL_video.h:589
   pragma Import (C, SDL_LoadBMP_RW, "SDL_LoadBMP_RW");

   function SDL_SaveBMP_RW
     (surface : access SDL_Surface;
      dst : access SDL_SDL_rwops_h.SDL_RWops;
      freedst : int) return int;  -- ../include/SDL/SDL_video.h:599
   pragma Import (C, SDL_SaveBMP_RW, "SDL_SaveBMP_RW");

   function SDL_SetColorKey
     (surface : access SDL_Surface;
      flag : SDL_SDL_stdinc_h.Uint32;
      key : SDL_SDL_stdinc_h.Uint32) return int;  -- ../include/SDL/SDL_video.h:615
   pragma Import (C, SDL_SetColorKey, "SDL_SetColorKey");

   function SDL_SetAlpha
     (surface : access SDL_Surface;
      flag : SDL_SDL_stdinc_h.Uint32;
      alpha : SDL_SDL_stdinc_h.Uint8) return int;  -- ../include/SDL/SDL_video.h:633
   pragma Import (C, SDL_SetAlpha, "SDL_SetAlpha");

   function SDL_SetClipRect (surface : access SDL_Surface; rect : System.Address) return SDL_SDL_stdinc_h.SDL_bool;  -- ../include/SDL/SDL_video.h:647
   pragma Import (C, SDL_SetClipRect, "SDL_SetClipRect");

   procedure SDL_GetClipRect (surface : access SDL_Surface; rect : access SDL_Rect);  -- ../include/SDL/SDL_video.h:654
   pragma Import (C, SDL_GetClipRect, "SDL_GetClipRect");

   function SDL_ConvertSurface
     (src : access SDL_Surface;
      fmt : access SDL_PixelFormat;
      flags : SDL_SDL_stdinc_h.Uint32) return access SDL_Surface;  -- ../include/SDL/SDL_video.h:668
   pragma Import (C, SDL_ConvertSurface, "SDL_ConvertSurface");

   function SDL_UpperBlit
     (src : access SDL_Surface;
      srcrect : access SDL_Rect;
      dst : access SDL_Surface;
      dstrect : access SDL_Rect) return int;  -- ../include/SDL/SDL_video.h:748
   pragma Import (C, SDL_UpperBlit, "SDL_UpperBlit");

   function SDL_LowerBlit
     (src : access SDL_Surface;
      srcrect : access SDL_Rect;
      dst : access SDL_Surface;
      dstrect : access SDL_Rect) return int;  -- ../include/SDL/SDL_video.h:754
   pragma Import (C, SDL_LowerBlit, "SDL_LowerBlit");

   function SDL_FillRect
     (dst : access SDL_Surface;
      dstrect : access SDL_Rect;
      color : SDL_SDL_stdinc_h.Uint32) return int;  -- ../include/SDL/SDL_video.h:767
   pragma Import (C, SDL_FillRect, "SDL_FillRect");

   function SDL_DisplayFormat (surface : access SDL_Surface) return access SDL_Surface;  -- ../include/SDL/SDL_video.h:781
   pragma Import (C, SDL_DisplayFormat, "SDL_DisplayFormat");

   function SDL_DisplayFormatAlpha (surface : access SDL_Surface) return access SDL_Surface;  -- ../include/SDL/SDL_video.h:795
   pragma Import (C, SDL_DisplayFormatAlpha, "SDL_DisplayFormatAlpha");

   function SDL_CreateYUVOverlay
     (width : int;
      height : int;
      format : SDL_SDL_stdinc_h.Uint32;
      display : access SDL_Surface) return access SDL_Overlay;  -- ../include/SDL/SDL_video.h:807
   pragma Import (C, SDL_CreateYUVOverlay, "SDL_CreateYUVOverlay");

   function SDL_LockYUVOverlay (overlay : access SDL_Overlay) return int;  -- ../include/SDL/SDL_video.h:811
   pragma Import (C, SDL_LockYUVOverlay, "SDL_LockYUVOverlay");

   procedure SDL_UnlockYUVOverlay (overlay : access SDL_Overlay);  -- ../include/SDL/SDL_video.h:812
   pragma Import (C, SDL_UnlockYUVOverlay, "SDL_UnlockYUVOverlay");

   function SDL_DisplayYUVOverlay (overlay : access SDL_Overlay; dstrect : access SDL_Rect) return int;  -- ../include/SDL/SDL_video.h:820
   pragma Import (C, SDL_DisplayYUVOverlay, "SDL_DisplayYUVOverlay");

   procedure SDL_FreeYUVOverlay (overlay : access SDL_Overlay);  -- ../include/SDL/SDL_video.h:823
   pragma Import (C, SDL_FreeYUVOverlay, "SDL_FreeYUVOverlay");

   function SDL_GL_LoadLibrary (path : Interfaces.C.Strings.chars_ptr) return int;  -- ../include/SDL/SDL_video.h:837
   pragma Import (C, SDL_GL_LoadLibrary, "SDL_GL_LoadLibrary");

   function SDL_GL_GetProcAddress (proc : Interfaces.C.Strings.chars_ptr) return System.Address;  -- ../include/SDL/SDL_video.h:842
   pragma Import (C, SDL_GL_GetProcAddress, "SDL_GL_GetProcAddress");

   function SDL_GL_SetAttribute (attr : SDL_GLattr; value : int) return int;  -- ../include/SDL/SDL_video.h:847
   pragma Import (C, SDL_GL_SetAttribute, "SDL_GL_SetAttribute");

   function SDL_GL_GetAttribute (attr : SDL_GLattr; value : access int) return int;  -- ../include/SDL/SDL_video.h:858
   pragma Import (C, SDL_GL_GetAttribute, "SDL_GL_GetAttribute");

   procedure SDL_GL_SwapBuffers;  -- ../include/SDL/SDL_video.h:863
   pragma Import (C, SDL_GL_SwapBuffers, "SDL_GL_SwapBuffers");

   procedure SDL_GL_UpdateRects (numrects : int; rects : access SDL_Rect);  -- ../include/SDL/SDL_video.h:870
   pragma Import (C, SDL_GL_UpdateRects, "SDL_GL_UpdateRects");

   procedure SDL_GL_Lock;  -- ../include/SDL/SDL_video.h:871
   pragma Import (C, SDL_GL_Lock, "SDL_GL_Lock");

   procedure SDL_GL_Unlock;  -- ../include/SDL/SDL_video.h:872
   pragma Import (C, SDL_GL_Unlock, "SDL_GL_Unlock");

   procedure SDL_WM_SetCaption (title : Interfaces.C.Strings.chars_ptr; icon : Interfaces.C.Strings.chars_ptr);  -- ../include/SDL/SDL_video.h:885
   pragma Import (C, SDL_WM_SetCaption, "SDL_WM_SetCaption");

   procedure SDL_WM_GetCaption (title : System.Address; icon : System.Address);  -- ../include/SDL/SDL_video.h:889
   pragma Import (C, SDL_WM_GetCaption, "SDL_WM_GetCaption");

   procedure SDL_WM_SetIcon (icon : access SDL_Surface; mask : access SDL_SDL_stdinc_h.Uint8);  -- ../include/SDL/SDL_video.h:897
   pragma Import (C, SDL_WM_SetIcon, "SDL_WM_SetIcon");

   function SDL_WM_IconifyWindow return int;  -- ../include/SDL/SDL_video.h:904
   pragma Import (C, SDL_WM_IconifyWindow, "SDL_WM_IconifyWindow");

   function SDL_WM_ToggleFullScreen (surface : access SDL_Surface) return int;  -- ../include/SDL/SDL_video.h:921
   pragma Import (C, SDL_WM_ToggleFullScreen, "SDL_WM_ToggleFullScreen");

   subtype SDL_GrabMode is unsigned;
   SDL_GRAB_QUERY : constant SDL_GrabMode := -1;
   SDL_GRAB_OFF : constant SDL_GrabMode := 0;
   SDL_GRAB_ON : constant SDL_GrabMode := 1;
   SDL_GRAB_FULLSCREEN : constant SDL_GrabMode := 2;  -- ../include/SDL/SDL_video.h:928

   function SDL_WM_GrabInput (mode : SDL_GrabMode) return SDL_GrabMode;  -- ../include/SDL/SDL_video.h:937
   pragma Import (C, SDL_WM_GrabInput, "SDL_WM_GrabInput");

   function SDL_SoftStretch
     (src : access SDL_Surface;
      srcrect : access SDL_Rect;
      dst : access SDL_Surface;
      dstrect : access SDL_Rect) return int;  -- ../include/SDL/SDL_video.h:942
   pragma Import (C, SDL_SoftStretch, "SDL_SoftStretch");

end SDL_SDL_video_h;
