------------------------------------------------------------------------------
--          Copyright (C) 1995-2014, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- In particular,  you can freely  distribute your programs  built with the --
-- GNAT Pro compiler, including any required library run-time units,  using --
-- any licensing terms  of your choosing.  See the AdaCore Software License --
-- for full details.                                                        --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with stdint_h;
with Interfaces.C.Strings;
with umingw_h;
with System;
with vadefs_h;

package SDL_SDL_stdinc_h is

   --  arg-macro: function SDL_arraysize (array)
   --    return sizeof(array)/sizeof(array(0));
   --  arg-macro: procedure SDL_TABLESIZE (table)
   --    SDL_arraysize(table)
   --  arg-macro: procedure SDL_reinterpret_cast (type, expressioreinterpret_cast<type>(expression)
   --    reinterpret_cast<type>(expression)
   --  arg-macro: procedure SDL_static_cast (type, expressiostatic_cast<type>(expression)
   --    static_cast<type>(expression)
   --  unsupported macro: SDL_COMPILE_TIME_ASSERT(name,x) typedef int SDL_dummy_ ## name[(x) * 2 - 1]
   --  unsupported macro: SDL_malloc malloc
   --  unsupported macro: SDL_calloc calloc
   --  unsupported macro: SDL_realloc realloc
   --  unsupported macro: SDL_free free
   --  arg-macro: function SDL_stack_alloc (type, count)
   --    return type*)alloca(sizeof(type)*(count);
   --  unsupported macro: SDL_qsort qsort
   --  unsupported macro: SDL_abs abs
   --  arg-macro: function SDL_min (x, y)
   --    return ((x) < (y)) ? (x) : (y);
   --  arg-macro: function SDL_max (x, y)
   --    return ((x) > (y)) ? (x) : (y);
   --  arg-macro: procedure SDL_isdigit (X)
   --    isdigit(X)
   --  arg-macro: procedure SDL_isspace (X)
   --    isspace(X)
   --  arg-macro: procedure SDL_toupper (X)
   --    toupper(X)
   --  arg-macro: procedure SDL_tolower (X)
   --    tolower(X)
   --  unsupported macro: SDL_memset memset
   --  arg-macro: procedure SDL_memset4 (dst, val, len)
   --    do { int u0, u1, u2; __asm__ __volatile__ ( "cld" & ASCII.LF & "" & ASCII.HT & "" "rep ; stosl" & ASCII.LF & "" & ASCII.HT & "" : "=&D" (u0), "=&a" (u1), "=&c" (u2) : "0" (dst), "1" (val), "2" (SDL_static_cast(Uint32, len)) : "memory" ); } while(0)
   --  arg-macro: procedure SDL_memcpy (dst, src, len)
   --    do { int u0, u1, u2; __asm__ __volatile__ ( "cld" & ASCII.LF & "" & ASCII.HT & "" "rep ; movsl" & ASCII.LF & "" & ASCII.HT & "" "testb $2,%b4" & ASCII.LF & "" & ASCII.HT & "" "je 1f" & ASCII.LF & "" & ASCII.HT & "" "movsw" & ASCII.LF & "" "1:" & ASCII.HT & "testb $1,%b4" & ASCII.LF & "" & ASCII.HT & "" "je 2f" & ASCII.LF & "" & ASCII.HT & "" "movsb" & ASCII.LF & "" "2:" : "=&c" (u0), "=&D" (u1), "=&S" (u2) : "0" (SDL_static_cast(unsigned, len)/4), "q" (len), "1" (dst),"2" (src) : "memory" ); } while(0)
   --  arg-macro: procedure SDL_memcpy4 (dst, src, len)
   --    do { int ecx, edi, esi; __asm__ __volatile__ ( "cld" & ASCII.LF & "" & ASCII.HT & "" "rep ; movsl" : "=&c" (ecx), "=&D" (edi), "=&S" (esi) : "0" (SDL_static_cast(unsigned, len)), "1" (dst), "2" (src) : "memory" ); } while(0)
   --  arg-macro: procedure SDL_revcpy (dst, src, len)
   --    do { int u0, u1, u2; char *dstp := SDL_static_cast(char *, dst); char *srcp := SDL_static_cast(char *, src); int n := (len); if ( n >= 4 ) { __asm__ __volatile__ ( "std" & ASCII.LF & "" & ASCII.HT & "" "rep ; movsl" & ASCII.LF & "" & ASCII.HT & "" "cld" & ASCII.LF & "" & ASCII.HT & "" : "=&c" (u0), "=&D" (u1), "=&S" (u2) : "0" (n >> 2), "1" (dstp+(n-4)), "2" (srcp+(n-4)) : "memory" ); } switch (n and 3) { case 3: dstp(2) := srcp(2); case 2: dstp(1) := srcp(1); case 1: dstp(0) := srcp(0); break; default: break; } } while(0)
   --  unsupported macro: SDL_memmove memmove
   --  unsupported macro: SDL_memcmp memcmp
   --  unsupported macro: SDL_strlen strlen
   --  unsupported macro: SDL_strrev _strrev
   --  unsupported macro: SDL_strupr _strupr
   --  unsupported macro: SDL_strlwr _strlwr
   --  unsupported macro: SDL_strchr strchr
   --  unsupported macro: SDL_strrchr strrchr
   --  unsupported macro: SDL_strstr strstr
   --  unsupported macro: SDL_itoa itoa
   --  unsupported macro: SDL_ltoa _ltoa
   --  arg-macro: procedure SDL_uitoa (value, string, SDL_ultoa((long)value, string, radix)
   --    SDL_ultoa((long)value, string, radix)
   --  unsupported macro: SDL_ultoa _ultoa
   --  unsupported macro: SDL_strtol strtol
   --  unsupported macro: SDL_strtoul strtoul
   --  unsupported macro: SDL_strtoll strtoll
   --  unsupported macro: SDL_strtod strtod
   --  unsupported macro: SDL_atoi atoi
   --  unsupported macro: SDL_atof atof
   --  unsupported macro: SDL_strcmp strcmp
   --  unsupported macro: SDL_strncmp strncmp
   --  unsupported macro: SDL_strcasecmp _stricmp
   --  unsupported macro: SDL_strncasecmp _strnicmp
   --  unsupported macro: SDL_sscanf sscanf
   --  unsupported macro: SDL_ICONV_ERROR (size_t)-1
   --  unsupported macro: SDL_ICONV_E2BIG (size_t)-2
   --  unsupported macro: SDL_ICONV_EILSEQ (size_t)-3
   --  unsupported macro: SDL_ICONV_EINVAL (size_t)-4
   --  arg-macro: procedure SDL_iconv_utf8_locale (S)
   --    SDL_iconv_string("", "UTF-8", S, SDL_strlen(S)+1)
   --  arg-macro: function SDL_iconv_utf8_ucs2 (S)
   --    return Uint16 *)SDL_iconv_string("UCS-2", "UTF-8", S, SDL_strlen(S)+1;
   --  arg-macro: function SDL_iconv_utf8_ucs4 (S)
   --    return Uint32 *)SDL_iconv_string("UCS-4", "UTF-8", S, SDL_strlen(S)+1;
   type SDL_bool is
     (SDL_FALSE,
      SDL_TRUE);
   pragma Convention (C, SDL_bool);  -- ../include/SDL/SDL_stdinc.h:96

   subtype Sint8 is stdint_h.int8_t;  -- ../include/SDL/SDL_stdinc.h:98

   subtype Uint8 is stdint_h.uint8_t;  -- ../include/SDL/SDL_stdinc.h:99

   subtype Sint16 is stdint_h.int16_t;  -- ../include/SDL/SDL_stdinc.h:100

   subtype Uint16 is stdint_h.uint16_t;  -- ../include/SDL/SDL_stdinc.h:101

   subtype Sint32 is stdint_h.int32_t;  -- ../include/SDL/SDL_stdinc.h:102

   subtype Uint32 is stdint_h.uint32_t;  -- ../include/SDL/SDL_stdinc.h:103

   subtype Sint64 is stdint_h.int64_t;  -- ../include/SDL/SDL_stdinc.h:106

   subtype Uint64 is stdint_h.uint64_t;  -- ../include/SDL/SDL_stdinc.h:108

   type SDL_dummy_uint8 is array (0 .. 0) of aliased int;  -- ../include/SDL/SDL_stdinc.h:125

   type SDL_dummy_sint8 is array (0 .. 0) of aliased int;  -- ../include/SDL/SDL_stdinc.h:126

   type SDL_dummy_uint16 is array (0 .. 0) of aliased int;  -- ../include/SDL/SDL_stdinc.h:127

   type SDL_dummy_sint16 is array (0 .. 0) of aliased int;  -- ../include/SDL/SDL_stdinc.h:128

   type SDL_dummy_uint32 is array (0 .. 0) of aliased int;  -- ../include/SDL/SDL_stdinc.h:129

   type SDL_dummy_sint32 is array (0 .. 0) of aliased int;  -- ../include/SDL/SDL_stdinc.h:130

   type SDL_dummy_uint64 is array (0 .. 0) of aliased int;  -- ../include/SDL/SDL_stdinc.h:131

   type SDL_dummy_sint64 is array (0 .. 0) of aliased int;  -- ../include/SDL/SDL_stdinc.h:132

   type SDL_DUMMY_ENUM is
     (DUMMY_ENUM_VALUE);
   pragma Convention (C, SDL_DUMMY_ENUM);  -- ../include/SDL/SDL_stdinc.h:148

   type SDL_dummy_enum_A is array (0 .. 0) of aliased int;  -- ../include/SDL/SDL_stdinc.h:151

   function SDL_getenv (name : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- ../include/SDL/SDL_stdinc.h:218
   pragma Import (C, SDL_getenv, "SDL_getenv");

   function SDL_putenv (variable : Interfaces.C.Strings.chars_ptr) return int;  -- ../include/SDL/SDL_stdinc.h:224

   function SDL_strlcpy
     (dst : Interfaces.C.Strings.chars_ptr;
      src : Interfaces.C.Strings.chars_ptr;
      maxlen : umingw_h.size_t) return umingw_h.size_t;  -- ../include/SDL/SDL_stdinc.h:403
   pragma Import (C, SDL_strlcpy, "SDL_strlcpy");

   function SDL_strlcat
     (dst : Interfaces.C.Strings.chars_ptr;
      src : Interfaces.C.Strings.chars_ptr;
      maxlen : umingw_h.size_t) return umingw_h.size_t;  -- ../include/SDL/SDL_stdinc.h:409
   pragma Import (C, SDL_strlcat, "SDL_strlcat");

   function SDL_strdup (string : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- ../include/SDL/SDL_stdinc.h:415
   pragma Import (C, SDL_strdup, "SDL_strdup");

   function SDL_lltoa
     (value : Sint64;
      string : Interfaces.C.Strings.chars_ptr;
      radix : int) return Interfaces.C.Strings.chars_ptr;  -- ../include/SDL/SDL_stdinc.h:499
   pragma Import (C, SDL_lltoa, "SDL_lltoa");

   function SDL_ulltoa
     (value : Uint64;
      string : Interfaces.C.Strings.chars_ptr;
      radix : int) return Interfaces.C.Strings.chars_ptr;  -- ../include/SDL/SDL_stdinc.h:505
   pragma Import (C, SDL_ulltoa, "SDL_ulltoa");

   function SDL_strtoull
     (string : Interfaces.C.Strings.chars_ptr;
      endp : System.Address;
      base : int) return Uint64;  -- ../include/SDL/SDL_stdinc.h:517
   pragma Import (C, SDL_strtoull, "SDL_strtoull");

   function SDL_snprintf
     (text : Interfaces.C.Strings.chars_ptr;
      maxlen : umingw_h.size_t;
      fmt : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- ../include/SDL/SDL_stdinc.h:577
   pragma Import (C, SDL_snprintf, "SDL_snprintf");

   function SDL_vsnprintf
     (text : Interfaces.C.Strings.chars_ptr;
      maxlen : umingw_h.size_t;
      fmt : Interfaces.C.Strings.chars_ptr;
      ap : vadefs_h.va_list) return int;  -- ../include/SDL/SDL_stdinc.h:583
   pragma Import (C, SDL_vsnprintf, "SDL_vsnprintf");

   --  skipped empty struct u_SDL_iconv_t

   type SDL_iconv_t is new System.Address;  -- ../include/SDL/SDL_stdinc.h:601

   function SDL_iconv_open (tocode : Interfaces.C.Strings.chars_ptr; fromcode : Interfaces.C.Strings.chars_ptr) return SDL_iconv_t;  -- ../include/SDL/SDL_stdinc.h:602
   pragma Import (C, SDL_iconv_open, "SDL_iconv_open");

   function SDL_iconv_close (cd : SDL_iconv_t) return int;  -- ../include/SDL/SDL_stdinc.h:603
   pragma Import (C, SDL_iconv_close, "SDL_iconv_close");

   function SDL_iconv
     (cd : SDL_iconv_t;
      inbuf : System.Address;
      inbytesleft : access umingw_h.size_t;
      outbuf : System.Address;
      outbytesleft : access umingw_h.size_t) return umingw_h.size_t;  -- ../include/SDL/SDL_stdinc.h:605
   pragma Import (C, SDL_iconv, "SDL_iconv");

   function SDL_iconv_string
     (tocode : Interfaces.C.Strings.chars_ptr;
      fromcode : Interfaces.C.Strings.chars_ptr;
      inbuf : Interfaces.C.Strings.chars_ptr;
      inbytesleft : umingw_h.size_t) return Interfaces.C.Strings.chars_ptr;  -- ../include/SDL/SDL_stdinc.h:609
   pragma Import (C, SDL_iconv_string, "SDL_iconv_string");

end SDL_SDL_stdinc_h;
