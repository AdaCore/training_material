pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
limited with SDL_SDL_version_h;
with Interfaces.C.Strings;
with System;
limited with SDL_SDL_rwops_h;
with SDL_SDL_stdinc_h;
with SDL_SDL_video_h;

package SDL_SDL_ttf_h is


   SDL_TTF_MAJOR_VERSION : constant := 2;  --  ../include/SDL/SDL_ttf.h:40
   SDL_TTF_MINOR_VERSION : constant := 0;  --  ../include/SDL/SDL_ttf.h:41
   SDL_TTF_PATCHLEVEL : constant := 11;  --  ../include/SDL/SDL_ttf.h:42
   --  arg-macro: procedure SDL_TTF_VERSION (X)
   --    { (X).major := SDL_TTF_MAJOR_VERSION; (X).minor := SDL_TTF_MINOR_VERSION; (X).patch := SDL_TTF_PATCHLEVEL; }
   --  unsupported macro: TTF_MAJOR_VERSION SDL_TTF_MAJOR_VERSION
   --  unsupported macro: TTF_MINOR_VERSION SDL_TTF_MINOR_VERSION
   --  unsupported macro: TTF_PATCHLEVEL SDL_TTF_PATCHLEVEL
   --  arg-macro: procedure TTF_VERSION (X)
   --    SDL_TTF_VERSION(X)

   UNICODE_BOM_NATIVE : constant := 16#FEFF#;  --  ../include/SDL/SDL_ttf.h:67
   UNICODE_BOM_SWAPPED : constant := 16#FFFE#;  --  ../include/SDL/SDL_ttf.h:68

   TTF_STYLE_NORMAL : constant := 16#00#;  --  ../include/SDL/SDL_ttf.h:92
   TTF_STYLE_BOLD : constant := 16#01#;  --  ../include/SDL/SDL_ttf.h:93
   TTF_STYLE_ITALIC : constant := 16#02#;  --  ../include/SDL/SDL_ttf.h:94
   TTF_STYLE_UNDERLINE : constant := 16#04#;  --  ../include/SDL/SDL_ttf.h:95
   TTF_STYLE_STRIKETHROUGH : constant := 16#08#;  --  ../include/SDL/SDL_ttf.h:96

   TTF_HINTING_NORMAL : constant := 0;  --  ../include/SDL/SDL_ttf.h:103
   TTF_HINTING_LIGHT : constant := 1;  --  ../include/SDL/SDL_ttf.h:104
   TTF_HINTING_MONO : constant := 2;  --  ../include/SDL/SDL_ttf.h:105
   TTF_HINTING_NONE : constant := 3;  --  ../include/SDL/SDL_ttf.h:106
   --  arg-macro: procedure TTF_RenderText (font, text, fg,TTF_RenderText_Shaded(font, text, fg, bg)
   --    TTF_RenderText_Shaded(font, text, fg, bg)
   --  arg-macro: procedure TTF_RenderUTF8 (font, text, fg,TTF_RenderUTF8_Shaded(font, text, fg, bg)
   --    TTF_RenderUTF8_Shaded(font, text, fg, bg)
   --  arg-macro: procedure TTF_RenderUNICODE (font, text, fg,TTF_RenderUNICODE_Shaded(font, text, fg, bg)
   --    TTF_RenderUNICODE_Shaded(font, text, fg, bg)
   --  unsupported macro: TTF_SetError SDL_SetError
   --  unsupported macro: TTF_GetError SDL_GetError

   function TTF_Linked_Version return access constant SDL_SDL_version_h.SDL_version;  -- ../include/SDL/SDL_ttf.h:64
   pragma Import (C, TTF_Linked_Version, "TTF_Linked_Version");

   procedure TTF_ByteSwappedUNICODE (swapped : int);  -- ../include/SDL/SDL_ttf.h:74
   pragma Import (C, TTF_ByteSwappedUNICODE, "TTF_ByteSwappedUNICODE");

   --  skipped empty struct u_TTF_Font

   --  skipped empty struct TTF_Font

   function TTF_Init return int;  -- ../include/SDL/SDL_ttf.h:80
   pragma Import (C, TTF_Init, "TTF_Init");

   function TTF_OpenFont (file : Interfaces.C.Strings.chars_ptr; ptsize : int) return System.Address;  -- ../include/SDL/SDL_ttf.h:86
   pragma Import (C, TTF_OpenFont, "TTF_OpenFont");

   function TTF_OpenFontIndex
     (file : Interfaces.C.Strings.chars_ptr;
      ptsize : int;
      index : long) return System.Address;  -- ../include/SDL/SDL_ttf.h:87
   pragma Import (C, TTF_OpenFontIndex, "TTF_OpenFontIndex");

   function TTF_OpenFontRW
     (src : access SDL_SDL_rwops_h.SDL_RWops;
      freesrc : int;
      ptsize : int) return System.Address;  -- ../include/SDL/SDL_ttf.h:88
   pragma Import (C, TTF_OpenFontRW, "TTF_OpenFontRW");

   function TTF_OpenFontIndexRW
     (src : access SDL_SDL_rwops_h.SDL_RWops;
      freesrc : int;
      ptsize : int;
      index : long) return System.Address;  -- ../include/SDL/SDL_ttf.h:89
   pragma Import (C, TTF_OpenFontIndexRW, "TTF_OpenFontIndexRW");

   function TTF_GetFontStyle (font : System.Address) return int;  -- ../include/SDL/SDL_ttf.h:97
   pragma Import (C, TTF_GetFontStyle, "TTF_GetFontStyle");

   procedure TTF_SetFontStyle (font : System.Address; style : int);  -- ../include/SDL/SDL_ttf.h:98
   pragma Import (C, TTF_SetFontStyle, "TTF_SetFontStyle");

   function TTF_GetFontOutline (font : System.Address) return int;  -- ../include/SDL/SDL_ttf.h:99
   pragma Import (C, TTF_GetFontOutline, "TTF_GetFontOutline");

   procedure TTF_SetFontOutline (font : System.Address; outline : int);  -- ../include/SDL/SDL_ttf.h:100
   pragma Import (C, TTF_SetFontOutline, "TTF_SetFontOutline");

   function TTF_GetFontHinting (font : System.Address) return int;  -- ../include/SDL/SDL_ttf.h:107
   pragma Import (C, TTF_GetFontHinting, "TTF_GetFontHinting");

   procedure TTF_SetFontHinting (font : System.Address; hinting : int);  -- ../include/SDL/SDL_ttf.h:108
   pragma Import (C, TTF_SetFontHinting, "TTF_SetFontHinting");

   function TTF_FontHeight (font : System.Address) return int;  -- ../include/SDL/SDL_ttf.h:111
   pragma Import (C, TTF_FontHeight, "TTF_FontHeight");

   function TTF_FontAscent (font : System.Address) return int;  -- ../include/SDL/SDL_ttf.h:116
   pragma Import (C, TTF_FontAscent, "TTF_FontAscent");

   function TTF_FontDescent (font : System.Address) return int;  -- ../include/SDL/SDL_ttf.h:121
   pragma Import (C, TTF_FontDescent, "TTF_FontDescent");

   function TTF_FontLineSkip (font : System.Address) return int;  -- ../include/SDL/SDL_ttf.h:124
   pragma Import (C, TTF_FontLineSkip, "TTF_FontLineSkip");

   function TTF_GetFontKerning (font : System.Address) return int;  -- ../include/SDL/SDL_ttf.h:127
   pragma Import (C, TTF_GetFontKerning, "TTF_GetFontKerning");

   procedure TTF_SetFontKerning (font : System.Address; allowed : int);  -- ../include/SDL/SDL_ttf.h:128
   pragma Import (C, TTF_SetFontKerning, "TTF_SetFontKerning");

   function TTF_FontFaces (font : System.Address) return long;  -- ../include/SDL/SDL_ttf.h:131
   pragma Import (C, TTF_FontFaces, "TTF_FontFaces");

   function TTF_FontFaceIsFixedWidth (font : System.Address) return int;  -- ../include/SDL/SDL_ttf.h:134
   pragma Import (C, TTF_FontFaceIsFixedWidth, "TTF_FontFaceIsFixedWidth");

   function TTF_FontFaceFamilyName (font : System.Address) return Interfaces.C.Strings.chars_ptr;  -- ../include/SDL/SDL_ttf.h:135
   pragma Import (C, TTF_FontFaceFamilyName, "TTF_FontFaceFamilyName");

   function TTF_FontFaceStyleName (font : System.Address) return Interfaces.C.Strings.chars_ptr;  -- ../include/SDL/SDL_ttf.h:136
   pragma Import (C, TTF_FontFaceStyleName, "TTF_FontFaceStyleName");

   function TTF_GlyphIsProvided (font : System.Address; ch : SDL_SDL_stdinc_h.Uint16) return int;  -- ../include/SDL/SDL_ttf.h:139
   pragma Import (C, TTF_GlyphIsProvided, "TTF_GlyphIsProvided");

   function TTF_GlyphMetrics
     (font : System.Address;
      ch : SDL_SDL_stdinc_h.Uint16;
      minx : access int;
      maxx : access int;
      miny : access int;
      maxy : access int;
      advance : access int) return int;  -- ../include/SDL/SDL_ttf.h:145
   pragma Import (C, TTF_GlyphMetrics, "TTF_GlyphMetrics");

   function TTF_SizeText
     (font : System.Address;
      text : Interfaces.C.Strings.chars_ptr;
      w : access int;
      h : access int) return int;  -- ../include/SDL/SDL_ttf.h:150
   pragma Import (C, TTF_SizeText, "TTF_SizeText");

   function TTF_SizeUTF8
     (font : System.Address;
      text : Interfaces.C.Strings.chars_ptr;
      w : access int;
      h : access int) return int;  -- ../include/SDL/SDL_ttf.h:151
   pragma Import (C, TTF_SizeUTF8, "TTF_SizeUTF8");

   function TTF_SizeUNICODE
     (font : System.Address;
      text : access SDL_SDL_stdinc_h.Uint16;
      w : access int;
      h : access int) return int;  -- ../include/SDL/SDL_ttf.h:152
   pragma Import (C, TTF_SizeUNICODE, "TTF_SizeUNICODE");

   function TTF_RenderText_Solid
     (font : System.Address;
      text : Interfaces.C.Strings.chars_ptr;
      fg : SDL_SDL_video_h.SDL_Color) return access SDL_SDL_video_h.SDL_Surface;  -- ../include/SDL/SDL_ttf.h:160
   pragma Import (C, TTF_RenderText_Solid, "TTF_RenderText_Solid");

   function TTF_RenderUTF8_Solid
     (font : System.Address;
      text : Interfaces.C.Strings.chars_ptr;
      fg : SDL_SDL_video_h.SDL_Color) return access SDL_SDL_video_h.SDL_Surface;  -- ../include/SDL/SDL_ttf.h:162
   pragma Import (C, TTF_RenderUTF8_Solid, "TTF_RenderUTF8_Solid");

   function TTF_RenderUNICODE_Solid
     (font : System.Address;
      text : access SDL_SDL_stdinc_h.Uint16;
      fg : SDL_SDL_video_h.SDL_Color) return access SDL_SDL_video_h.SDL_Surface;  -- ../include/SDL/SDL_ttf.h:164
   pragma Import (C, TTF_RenderUNICODE_Solid, "TTF_RenderUNICODE_Solid");

   function TTF_RenderGlyph_Solid
     (font : System.Address;
      ch : SDL_SDL_stdinc_h.Uint16;
      fg : SDL_SDL_video_h.SDL_Color) return access SDL_SDL_video_h.SDL_Surface;  -- ../include/SDL/SDL_ttf.h:174
   pragma Import (C, TTF_RenderGlyph_Solid, "TTF_RenderGlyph_Solid");

   function TTF_RenderText_Shaded
     (font : System.Address;
      text : Interfaces.C.Strings.chars_ptr;
      fg : SDL_SDL_video_h.SDL_Color;
      bg : SDL_SDL_video_h.SDL_Color) return access SDL_SDL_video_h.SDL_Surface;  -- ../include/SDL/SDL_ttf.h:182
   pragma Import (C, TTF_RenderText_Shaded, "TTF_RenderText_Shaded");

   function TTF_RenderUTF8_Shaded
     (font : System.Address;
      text : Interfaces.C.Strings.chars_ptr;
      fg : SDL_SDL_video_h.SDL_Color;
      bg : SDL_SDL_video_h.SDL_Color) return access SDL_SDL_video_h.SDL_Surface;  -- ../include/SDL/SDL_ttf.h:184
   pragma Import (C, TTF_RenderUTF8_Shaded, "TTF_RenderUTF8_Shaded");

   function TTF_RenderUNICODE_Shaded
     (font : System.Address;
      text : access SDL_SDL_stdinc_h.Uint16;
      fg : SDL_SDL_video_h.SDL_Color;
      bg : SDL_SDL_video_h.SDL_Color) return access SDL_SDL_video_h.SDL_Surface;  -- ../include/SDL/SDL_ttf.h:186
   pragma Import (C, TTF_RenderUNICODE_Shaded, "TTF_RenderUNICODE_Shaded");

   function TTF_RenderGlyph_Shaded
     (font : System.Address;
      ch : SDL_SDL_stdinc_h.Uint16;
      fg : SDL_SDL_video_h.SDL_Color;
      bg : SDL_SDL_video_h.SDL_Color) return access SDL_SDL_video_h.SDL_Surface;  -- ../include/SDL/SDL_ttf.h:196
   pragma Import (C, TTF_RenderGlyph_Shaded, "TTF_RenderGlyph_Shaded");

   function TTF_RenderText_Blended
     (font : System.Address;
      text : Interfaces.C.Strings.chars_ptr;
      fg : SDL_SDL_video_h.SDL_Color) return access SDL_SDL_video_h.SDL_Surface;  -- ../include/SDL/SDL_ttf.h:203
   pragma Import (C, TTF_RenderText_Blended, "TTF_RenderText_Blended");

   function TTF_RenderUTF8_Blended
     (font : System.Address;
      text : Interfaces.C.Strings.chars_ptr;
      fg : SDL_SDL_video_h.SDL_Color) return access SDL_SDL_video_h.SDL_Surface;  -- ../include/SDL/SDL_ttf.h:205
   pragma Import (C, TTF_RenderUTF8_Blended, "TTF_RenderUTF8_Blended");

   function TTF_RenderUNICODE_Blended
     (font : System.Address;
      text : access SDL_SDL_stdinc_h.Uint16;
      fg : SDL_SDL_video_h.SDL_Color) return access SDL_SDL_video_h.SDL_Surface;  -- ../include/SDL/SDL_ttf.h:207
   pragma Import (C, TTF_RenderUNICODE_Blended, "TTF_RenderUNICODE_Blended");

   function TTF_RenderGlyph_Blended
     (font : System.Address;
      ch : SDL_SDL_stdinc_h.Uint16;
      fg : SDL_SDL_video_h.SDL_Color) return access SDL_SDL_video_h.SDL_Surface;  -- ../include/SDL/SDL_ttf.h:216
   pragma Import (C, TTF_RenderGlyph_Blended, "TTF_RenderGlyph_Blended");

   procedure TTF_CloseFont (font : System.Address);  -- ../include/SDL/SDL_ttf.h:228
   pragma Import (C, TTF_CloseFont, "TTF_CloseFont");

   procedure TTF_Quit;  -- ../include/SDL/SDL_ttf.h:231
   pragma Import (C, TTF_Quit, "TTF_Quit");

   function TTF_WasInit return int;  -- ../include/SDL/SDL_ttf.h:234
   pragma Import (C, TTF_WasInit, "TTF_WasInit");

   function TTF_GetFontKerningSize
     (font : System.Address;
      prev_index : int;
      index : int) return int;  -- ../include/SDL/SDL_ttf.h:237
   pragma Import (C, TTF_GetFontKerningSize, "TTF_GetFontKerningSize");

end SDL_SDL_ttf_h;
