pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with stdint_h;
with stddef_h;
with System;
with Interfaces.C.Strings;
with stdarg_h;

package SDL_stdinc_h is

   --  arg-macro: function SDL_arraysize (array)
   --    return sizeof(array)/sizeof(array(0));
   --  arg-macro: procedure SDL_TABLESIZE (table)
   --    SDL_arraysize(table)
   --  unsupported macro: SDL_STRINGIFY_ARG(arg) #arg
   --  arg-macro: procedure SDL_reinterpret_cast (type, expression)
   --    reinterpret_cast<type>(expression)
   --  arg-macro: procedure SDL_static_cast (type, expression)
   --    static_cast<type>(expression)
   --  arg-macro: procedure SDL_const_cast (type, expression)
   --    const_cast<type>(expression)
   --  arg-macro: function SDL_FOURCC (A, B, C, D)
   --    return (SDL_static_cast(Uint32, SDL_static_cast(Uint8, (A))) << 0) or (SDL_static_cast(Uint32, SDL_static_cast(Uint8, (B))) << 8) or (SDL_static_cast(Uint32, SDL_static_cast(Uint8, (C))) << 16) or (SDL_static_cast(Uint32, SDL_static_cast(Uint8, (D))) << 24);
   --  unsupported macro: SDL_MAX_SINT8 ((Sint8)0x7F)
   --  unsupported macro: SDL_MIN_SINT8 ((Sint8)(~0x7F))
   --  unsupported macro: SDL_MAX_UINT8 ((Uint8)0xFF)
   --  unsupported macro: SDL_MIN_UINT8 ((Uint8)0x00)
   --  unsupported macro: SDL_MAX_SINT16 ((Sint16)0x7FFF)
   --  unsupported macro: SDL_MIN_SINT16 ((Sint16)(~0x7FFF))
   --  unsupported macro: SDL_MAX_UINT16 ((Uint16)0xFFFF)
   --  unsupported macro: SDL_MIN_UINT16 ((Uint16)0x0000)
   --  unsupported macro: SDL_MAX_SINT32 ((Sint32)0x7FFFFFFF)
   --  unsupported macro: SDL_MIN_SINT32 ((Sint32)(~0x7FFFFFFF))
   --  unsupported macro: SDL_MAX_UINT32 ((Uint32)0xFFFFFFFFu)
   --  unsupported macro: SDL_MIN_UINT32 ((Uint32)0x00000000)
   --  unsupported macro: SDL_MAX_SINT64 ((Sint64)0x7FFFFFFFFFFFFFFFll)
   --  unsupported macro: SDL_MIN_SINT64 ((Sint64)(~0x7FFFFFFFFFFFFFFFll))
   --  unsupported macro: SDL_MAX_UINT64 ((Uint64)0xFFFFFFFFFFFFFFFFull)
   --  unsupported macro: SDL_MIN_UINT64 ((Uint64)(0x0000000000000000ull))
   SDL_PRIs64 : aliased constant String := "I64d" & ASCII.NUL;  --  ..\SDL2_tmp\SDL_stdinc.h:227

   SDL_PRIu64 : aliased constant String := "I64u" & ASCII.NUL;  --  ..\SDL2_tmp\SDL_stdinc.h:238

   SDL_PRIx64 : aliased constant String := "I64x" & ASCII.NUL;  --  ..\SDL2_tmp\SDL_stdinc.h:249

   -- manual fixed because Ada is case insensitive !
   --SDL_PRIX64 : aliased constant String := "I64X" & ASCII.NUL;  --  ..\SDL2_tmp\SDL_stdinc.h:260
   
   --  arg-macro: procedure SDL_PRINTF_VARARG_FUNC (fmtargnumber)
   --    __attribute__ (( format( __printf__, fmtargnumber, fmtargnumber+1 )))
   --  arg-macro: procedure SDL_SCANF_VARARG_FUNC (fmtargnumber)
   --    __attribute__ (( format( __scanf__, fmtargnumber, fmtargnumber+1 )))
   --  unsupported macro: SDL_COMPILE_TIME_ASSERT(name,x) typedef int SDL_compile_time_assert_ ## name[(x) * 2 - 1]
   --  arg-macro: function SDL_stack_alloc (type, count)
   --    return type*)SDL_malloc(sizeof(type)*(count);
   --  arg-macro: procedure SDL_stack_free (data)
   --    SDL_free(data)
   --  arg-macro: function SDL_min (x, y)
   --    return ((x) < (y)) ? (x) : (y);
   --  arg-macro: function SDL_max (x, y)
   --    return ((x) > (y)) ? (x) : (y);
   --  arg-macro: procedure SDL_zero (x)
   --    SDL_memset(and(x), 0, sizeof((x)))
   --  arg-macro: procedure SDL_zerop (x)
   --    SDL_memset((x), 0, sizeof(*(x)))

   M_PI : constant := 3.14159265358979323846264338327950288;  --  ..\SDL2_tmp\SDL_stdinc.h:499
   --  unsupported macro: SDL_ICONV_ERROR (size_t)-1
   --  unsupported macro: SDL_ICONV_E2BIG (size_t)-2
   --  unsupported macro: SDL_ICONV_EILSEQ (size_t)-3
   --  unsupported macro: SDL_ICONV_EINVAL (size_t)-4
   --  arg-macro: procedure SDL_iconv_utf8_locale (S)
   --    SDL_iconv_string("", & "UTF-8", S, SDL_strlen(S)+1)
   --  arg-macro: function SDL_iconv_utf8_ucs2 (S)
   --    return Uint16 *)SDL_iconv_string("UCS-2-INTERNAL", & "UTF-8", S, SDL_strlen(S)+1;
   --  arg-macro: function SDL_iconv_utf8_ucs4 (S)
   --    return Uint32 *)SDL_iconv_string("UCS-4-INTERNAL", & "UTF-8", S, SDL_strlen(S)+1;

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
  -- *  \file SDL_stdinc.h
  -- *
  -- *  This is a general header that includes C language support.
  --  

  -- Defining _USE_MATH_DEFINES is required to get M_PI to be defined on
  --   WinRT.  See http://msdn.microsoft.com/en-us/library/4hwaceh6.aspx
  --   for more information.
  -- 

  --*
  -- *  The number of elements in an array.
  --  

  --*
  -- *  Macro useful for building other macros with strings in them
  -- *
  -- *  e.g. #define LOG_ERROR(X) OutputDebugString(SDL_STRINGIFY_ARG(__FUNCTION__) ": " X "\n")
  --  

  --*
  -- *  \name Cast operators
  -- *
  -- *  Use proper C++ casts when compiled as C++ to be compatible with the option
  -- *  -Wold-style-cast of GCC (and -Werror=old-style-cast in GCC 4.2 and above).
  --  

  -- @{  
  -- @}  
  -- Cast operators  
  -- Define a four character code as a Uint32  
  --*
  -- *  \name Basic data types
  --  

  -- @{  
  -- ARM's compiler throws warnings if we use an enum: like "SDL_bool x = a < b;"  
   type SDL_bool is 
     (SDL_FALSE,
      SDL_TRUE);
   pragma Convention (C, SDL_bool);  -- ..\SDL2_tmp\SDL_stdinc.h:165

  --*
  -- * \brief A signed 8-bit integer type.
  --  

   subtype Sint8 is stdint_h.int8_t;  -- ..\SDL2_tmp\SDL_stdinc.h:173

  --*
  -- * \brief An unsigned 8-bit integer type.
  --  

   subtype Uint8 is stdint_h.uint8_t;  -- ..\SDL2_tmp\SDL_stdinc.h:179

  --*
  -- * \brief A signed 16-bit integer type.
  --  

   subtype Sint16 is stdint_h.int16_t;  -- ..\SDL2_tmp\SDL_stdinc.h:185

  --*
  -- * \brief An unsigned 16-bit integer type.
  --  

   subtype Uint16 is stdint_h.uint16_t;  -- ..\SDL2_tmp\SDL_stdinc.h:191

  --*
  -- * \brief A signed 32-bit integer type.
  --  

   subtype Sint32 is stdint_h.int32_t;  -- ..\SDL2_tmp\SDL_stdinc.h:197

  --*
  -- * \brief An unsigned 32-bit integer type.
  --  

   subtype Uint32 is stdint_h.uint32_t;  -- ..\SDL2_tmp\SDL_stdinc.h:203

  --*
  -- * \brief A signed 64-bit integer type.
  --  

   subtype Sint64 is stdint_h.int64_t;  -- ..\SDL2_tmp\SDL_stdinc.h:210

  --*
  -- * \brief An unsigned 64-bit integer type.
  --  

   subtype Uint64 is stdint_h.uint64_t;  -- ..\SDL2_tmp\SDL_stdinc.h:216

  -- @}  
  -- Basic data types  
  -- Make sure we have macros for printing 64 bit values.
  -- * <stdint.h> should define these but this is not true all platforms.
  -- * (for example win32)  

  -- Annotations to help code analysis tools  
  --* \cond  
   type SDL_compile_time_assert_uint8 is array (0 .. 0) of aliased int;  -- ..\SDL2_tmp\SDL_stdinc.h:316

   type SDL_compile_time_assert_sint8 is array (0 .. 0) of aliased int;  -- ..\SDL2_tmp\SDL_stdinc.h:317

   type SDL_compile_time_assert_uint16 is array (0 .. 0) of aliased int;  -- ..\SDL2_tmp\SDL_stdinc.h:318

   type SDL_compile_time_assert_sint16 is array (0 .. 0) of aliased int;  -- ..\SDL2_tmp\SDL_stdinc.h:319

   type SDL_compile_time_assert_uint32 is array (0 .. 0) of aliased int;  -- ..\SDL2_tmp\SDL_stdinc.h:320

   type SDL_compile_time_assert_sint32 is array (0 .. 0) of aliased int;  -- ..\SDL2_tmp\SDL_stdinc.h:321

   type SDL_compile_time_assert_uint64 is array (0 .. 0) of aliased int;  -- ..\SDL2_tmp\SDL_stdinc.h:322

   type SDL_compile_time_assert_sint64 is array (0 .. 0) of aliased int;  -- ..\SDL2_tmp\SDL_stdinc.h:323

  --* \endcond  
  -- Check to make sure enums are the size of ints, for structure packing.
  --   For both Watcom C/C++ and Borland C/C++ the compiler option that makes
  --   enums having the size of an int must be enabled.
  --   This is "-b" for Borland C/C++ and "-ei" for Watcom C/C++ (v11).
  -- 

  --* \cond  
  -- TODO: include/SDL_stdinc.h:174: error: size of array 'SDL_dummy_enum' is negative  
   type SDL_DUMMY_ENUM is 
     (DUMMY_ENUM_VALUE);
   pragma Convention (C, SDL_DUMMY_ENUM);  -- ..\SDL2_tmp\SDL_stdinc.h:340

   type SDL_compile_time_assert_enum is array (0 .. 0) of aliased int;  -- ..\SDL2_tmp\SDL_stdinc.h:342

  --* \endcond  
  -- Set up for C function definitions, even when using C++  
   function SDL_malloc (size : stddef_h.size_t) return System.Address;  -- ..\SDL2_tmp\SDL_stdinc.h:361
   pragma Import (C, SDL_malloc, "SDL_malloc");

   function SDL_calloc (nmemb : stddef_h.size_t; size : stddef_h.size_t) return System.Address;  -- ..\SDL2_tmp\SDL_stdinc.h:362
   pragma Import (C, SDL_calloc, "SDL_calloc");

   function SDL_realloc (mem : System.Address; size : stddef_h.size_t) return System.Address;  -- ..\SDL2_tmp\SDL_stdinc.h:363
   pragma Import (C, SDL_realloc, "SDL_realloc");

   procedure SDL_free (mem : System.Address);  -- ..\SDL2_tmp\SDL_stdinc.h:364
   pragma Import (C, SDL_free, "SDL_free");

   type SDL_malloc_func is access function (arg1 : stddef_h.size_t) return System.Address;
   pragma Convention (C, SDL_malloc_func);  -- ..\SDL2_tmp\SDL_stdinc.h:366

   type SDL_calloc_func is access function (arg1 : stddef_h.size_t; arg2 : stddef_h.size_t) return System.Address;
   pragma Convention (C, SDL_calloc_func);  -- ..\SDL2_tmp\SDL_stdinc.h:367

   type SDL_realloc_func is access function (arg1 : System.Address; arg2 : stddef_h.size_t) return System.Address;
   pragma Convention (C, SDL_realloc_func);  -- ..\SDL2_tmp\SDL_stdinc.h:368

   type SDL_free_func is access procedure (arg1 : System.Address);
   pragma Convention (C, SDL_free_func);  -- ..\SDL2_tmp\SDL_stdinc.h:369

  --*
  -- *  \brief Get the current set of SDL memory functions
  --  

   procedure SDL_GetMemoryFunctions
     (malloc_func : System.Address;
      calloc_func : System.Address;
      realloc_func : System.Address;
      free_func : System.Address);  -- ..\SDL2_tmp\SDL_stdinc.h:374
   pragma Import (C, SDL_GetMemoryFunctions, "SDL_GetMemoryFunctions");

  --*
  -- *  \brief Replace SDL's memory allocation functions with a custom set
  -- *
  -- *  \note If you are replacing SDL's memory functions, you should call
  -- *        SDL_GetNumAllocations() and be very careful if it returns non-zero.
  -- *        That means that your free function will be called with memory
  -- *        allocated by the previous memory allocation functions.
  --  

   function SDL_SetMemoryFunctions
     (malloc_func : SDL_malloc_func;
      calloc_func : SDL_calloc_func;
      realloc_func : SDL_realloc_func;
      free_func : SDL_free_func) return int;  -- ..\SDL2_tmp\SDL_stdinc.h:387
   pragma Import (C, SDL_SetMemoryFunctions, "SDL_SetMemoryFunctions");

  --*
  -- *  \brief Get the number of outstanding (unfreed) allocations
  --  

   function SDL_GetNumAllocations return int;  -- ..\SDL2_tmp\SDL_stdinc.h:395
   pragma Import (C, SDL_GetNumAllocations, "SDL_GetNumAllocations");

   function SDL_getenv (name : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_stdinc.h:397
   pragma Import (C, SDL_getenv, "SDL_getenv");

   function SDL_setenv
     (name : Interfaces.C.Strings.chars_ptr;
      value : Interfaces.C.Strings.chars_ptr;
      overwrite : int) return int;  -- ..\SDL2_tmp\SDL_stdinc.h:398
   pragma Import (C, SDL_setenv, "SDL_setenv");

   procedure SDL_qsort
     (base : System.Address;
      nmemb : stddef_h.size_t;
      size : stddef_h.size_t;
      compare : access function (arg1 : System.Address; arg2 : System.Address) return int);  -- ..\SDL2_tmp\SDL_stdinc.h:400
   pragma Import (C, SDL_qsort, "SDL_qsort");

   function SDL_abs (x : int) return int;  -- ..\SDL2_tmp\SDL_stdinc.h:402
   pragma Import (C, SDL_abs, "SDL_abs");

  -- !!! FIXME: these have side effects. You probably shouldn't use them.  
  -- !!! FIXME: Maybe we do forceinline functions of SDL_mini, SDL_minf, etc?  
   function SDL_isdigit (x : int) return int;  -- ..\SDL2_tmp\SDL_stdinc.h:409
   pragma Import (C, SDL_isdigit, "SDL_isdigit");

   function SDL_isspace (x : int) return int;  -- ..\SDL2_tmp\SDL_stdinc.h:410
   pragma Import (C, SDL_isspace, "SDL_isspace");

   function SDL_toupper (x : int) return int;  -- ..\SDL2_tmp\SDL_stdinc.h:411
   pragma Import (C, SDL_toupper, "SDL_toupper");

   function SDL_tolower (x : int) return int;  -- ..\SDL2_tmp\SDL_stdinc.h:412
   pragma Import (C, SDL_tolower, "SDL_tolower");

   function SDL_memset
     (dst : System.Address;
      c : int;
      len : stddef_h.size_t) return System.Address;  -- ..\SDL2_tmp\SDL_stdinc.h:414
   pragma Import (C, SDL_memset, "SDL_memset");

  -- Note that memset() is a byte assignment and this is a 32-bit assignment, so they're not directly equivalent.  
   procedure SDL_memset4
     (dst : System.Address;
      val : Uint32;
      dwords : stddef_h.size_t);  -- ..\SDL2_tmp\SDL_stdinc.h:420
   pragma Import (C, SDL_memset4, "SDL_memset4");

  -- fallthrough  
  -- fallthrough  
  -- fallthrough  
  -- fallthrough  
   function SDL_memcpy
     (dst : System.Address;
      src : System.Address;
      len : stddef_h.size_t) return System.Address;  -- ..\SDL2_tmp\SDL_stdinc.h:448
   pragma Import (C, SDL_memcpy, "SDL_memcpy");

   function SDL_memmove
     (dst : System.Address;
      src : System.Address;
      len : stddef_h.size_t) return System.Address;  -- ..\SDL2_tmp\SDL_stdinc.h:450
   pragma Import (C, SDL_memmove, "SDL_memmove");

   function SDL_memcmp
     (s1 : System.Address;
      s2 : System.Address;
      len : stddef_h.size_t) return int;  -- ..\SDL2_tmp\SDL_stdinc.h:451
   pragma Import (C, SDL_memcmp, "SDL_memcmp");

   function SDL_wcsdup (wstr : access wchar_t) return access wchar_t;  -- ..\SDL2_tmp\SDL_stdinc.h:453
   pragma Import (C, SDL_wcsdup, "SDL_wcsdup");

   function SDL_wcslen (wstr : access wchar_t) return stddef_h.size_t;  -- ..\SDL2_tmp\SDL_stdinc.h:454
   pragma Import (C, SDL_wcslen, "SDL_wcslen");

   function SDL_wcslcpy
     (dst : access wchar_t;
      src : access wchar_t;
      maxlen : stddef_h.size_t) return stddef_h.size_t;  -- ..\SDL2_tmp\SDL_stdinc.h:455
   pragma Import (C, SDL_wcslcpy, "SDL_wcslcpy");

   function SDL_wcslcat
     (dst : access wchar_t;
      src : access wchar_t;
      maxlen : stddef_h.size_t) return stddef_h.size_t;  -- ..\SDL2_tmp\SDL_stdinc.h:456
   pragma Import (C, SDL_wcslcat, "SDL_wcslcat");

   function SDL_wcscmp (str1 : access wchar_t; str2 : access wchar_t) return int;  -- ..\SDL2_tmp\SDL_stdinc.h:457
   pragma Import (C, SDL_wcscmp, "SDL_wcscmp");

   function SDL_strlen (str : Interfaces.C.Strings.chars_ptr) return stddef_h.size_t;  -- ..\SDL2_tmp\SDL_stdinc.h:459
   pragma Import (C, SDL_strlen, "SDL_strlen");

   function SDL_strlcpy
     (dst : Interfaces.C.Strings.chars_ptr;
      src : Interfaces.C.Strings.chars_ptr;
      maxlen : stddef_h.size_t) return stddef_h.size_t;  -- ..\SDL2_tmp\SDL_stdinc.h:460
   pragma Import (C, SDL_strlcpy, "SDL_strlcpy");

   function SDL_utf8strlcpy
     (dst : Interfaces.C.Strings.chars_ptr;
      src : Interfaces.C.Strings.chars_ptr;
      dst_bytes : stddef_h.size_t) return stddef_h.size_t;  -- ..\SDL2_tmp\SDL_stdinc.h:461
   pragma Import (C, SDL_utf8strlcpy, "SDL_utf8strlcpy");

   function SDL_strlcat
     (dst : Interfaces.C.Strings.chars_ptr;
      src : Interfaces.C.Strings.chars_ptr;
      maxlen : stddef_h.size_t) return stddef_h.size_t;  -- ..\SDL2_tmp\SDL_stdinc.h:462
   pragma Import (C, SDL_strlcat, "SDL_strlcat");

   function SDL_strdup (str : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_stdinc.h:463
   pragma Import (C, SDL_strdup, "SDL_strdup");

   function SDL_strrev (str : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_stdinc.h:464
   pragma Import (C, SDL_strrev, "SDL_strrev");

   function SDL_strupr (str : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_stdinc.h:465
   pragma Import (C, SDL_strupr, "SDL_strupr");

   function SDL_strlwr (str : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_stdinc.h:466
   pragma Import (C, SDL_strlwr, "SDL_strlwr");

   function SDL_strchr (str : Interfaces.C.Strings.chars_ptr; c : int) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_stdinc.h:467
   pragma Import (C, SDL_strchr, "SDL_strchr");

   function SDL_strrchr (str : Interfaces.C.Strings.chars_ptr; c : int) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_stdinc.h:468
   pragma Import (C, SDL_strrchr, "SDL_strrchr");

   function SDL_strstr (haystack : Interfaces.C.Strings.chars_ptr; needle : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_stdinc.h:469
   pragma Import (C, SDL_strstr, "SDL_strstr");

   function SDL_utf8strlen (str : Interfaces.C.Strings.chars_ptr) return stddef_h.size_t;  -- ..\SDL2_tmp\SDL_stdinc.h:470
   pragma Import (C, SDL_utf8strlen, "SDL_utf8strlen");

   function SDL_itoa
     (value : int;
      str : Interfaces.C.Strings.chars_ptr;
      radix : int) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_stdinc.h:472
   pragma Import (C, SDL_itoa, "SDL_itoa");

   function SDL_uitoa
     (value : unsigned;
      str : Interfaces.C.Strings.chars_ptr;
      radix : int) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_stdinc.h:473
   pragma Import (C, SDL_uitoa, "SDL_uitoa");

   function SDL_ltoa
     (value : long;
      str : Interfaces.C.Strings.chars_ptr;
      radix : int) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_stdinc.h:474
   pragma Import (C, SDL_ltoa, "SDL_ltoa");

   function SDL_ultoa
     (value : unsigned_long;
      str : Interfaces.C.Strings.chars_ptr;
      radix : int) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_stdinc.h:475
   pragma Import (C, SDL_ultoa, "SDL_ultoa");

   function SDL_lltoa
     (value : Sint64;
      str : Interfaces.C.Strings.chars_ptr;
      radix : int) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_stdinc.h:476
   pragma Import (C, SDL_lltoa, "SDL_lltoa");

   function SDL_ulltoa
     (value : Uint64;
      str : Interfaces.C.Strings.chars_ptr;
      radix : int) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_stdinc.h:477
   pragma Import (C, SDL_ulltoa, "SDL_ulltoa");

   function SDL_atoi (str : Interfaces.C.Strings.chars_ptr) return int;  -- ..\SDL2_tmp\SDL_stdinc.h:479
   pragma Import (C, SDL_atoi, "SDL_atoi");

   function SDL_atof (str : Interfaces.C.Strings.chars_ptr) return double;  -- ..\SDL2_tmp\SDL_stdinc.h:480
   pragma Import (C, SDL_atof, "SDL_atof");

   function SDL_strtol
     (str : Interfaces.C.Strings.chars_ptr;
      endp : System.Address;
      base : int) return long;  -- ..\SDL2_tmp\SDL_stdinc.h:481
   pragma Import (C, SDL_strtol, "SDL_strtol");

   function SDL_strtoul
     (str : Interfaces.C.Strings.chars_ptr;
      endp : System.Address;
      base : int) return unsigned_long;  -- ..\SDL2_tmp\SDL_stdinc.h:482
   pragma Import (C, SDL_strtoul, "SDL_strtoul");

   function SDL_strtoll
     (str : Interfaces.C.Strings.chars_ptr;
      endp : System.Address;
      base : int) return Sint64;  -- ..\SDL2_tmp\SDL_stdinc.h:483
   pragma Import (C, SDL_strtoll, "SDL_strtoll");

   function SDL_strtoull
     (str : Interfaces.C.Strings.chars_ptr;
      endp : System.Address;
      base : int) return Uint64;  -- ..\SDL2_tmp\SDL_stdinc.h:484
   pragma Import (C, SDL_strtoull, "SDL_strtoull");

   function SDL_strtod (str : Interfaces.C.Strings.chars_ptr; endp : System.Address) return double;  -- ..\SDL2_tmp\SDL_stdinc.h:485
   pragma Import (C, SDL_strtod, "SDL_strtod");

   function SDL_strcmp (str1 : Interfaces.C.Strings.chars_ptr; str2 : Interfaces.C.Strings.chars_ptr) return int;  -- ..\SDL2_tmp\SDL_stdinc.h:487
   pragma Import (C, SDL_strcmp, "SDL_strcmp");

   function SDL_strncmp
     (str1 : Interfaces.C.Strings.chars_ptr;
      str2 : Interfaces.C.Strings.chars_ptr;
      maxlen : stddef_h.size_t) return int;  -- ..\SDL2_tmp\SDL_stdinc.h:488
   pragma Import (C, SDL_strncmp, "SDL_strncmp");

   function SDL_strcasecmp (str1 : Interfaces.C.Strings.chars_ptr; str2 : Interfaces.C.Strings.chars_ptr) return int;  -- ..\SDL2_tmp\SDL_stdinc.h:489
   pragma Import (C, SDL_strcasecmp, "SDL_strcasecmp");

   function SDL_strncasecmp
     (str1 : Interfaces.C.Strings.chars_ptr;
      str2 : Interfaces.C.Strings.chars_ptr;
      len : stddef_h.size_t) return int;  -- ..\SDL2_tmp\SDL_stdinc.h:490
   pragma Import (C, SDL_strncasecmp, "SDL_strncasecmp");

   function SDL_sscanf (text : Interfaces.C.Strings.chars_ptr; fmt : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- ..\SDL2_tmp\SDL_stdinc.h:492
   pragma Import (C, SDL_sscanf, "SDL_sscanf");

   function SDL_vsscanf
     (text : Interfaces.C.Strings.chars_ptr;
      fmt : Interfaces.C.Strings.chars_ptr;
      ap : stdarg_h.va_list) return int;  -- ..\SDL2_tmp\SDL_stdinc.h:493
   pragma Import (C, SDL_vsscanf, "SDL_vsscanf");

   function SDL_snprintf
     (text : Interfaces.C.Strings.chars_ptr;
      maxlen : stddef_h.size_t;
      fmt : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- ..\SDL2_tmp\SDL_stdinc.h:494
   pragma Import (C, SDL_snprintf, "SDL_snprintf");

   function SDL_vsnprintf
     (text : Interfaces.C.Strings.chars_ptr;
      maxlen : stddef_h.size_t;
      fmt : Interfaces.C.Strings.chars_ptr;
      ap : stdarg_h.va_list) return int;  -- ..\SDL2_tmp\SDL_stdinc.h:495
   pragma Import (C, SDL_vsnprintf, "SDL_vsnprintf");

   function SDL_acos (x : double) return double;  -- ..\SDL2_tmp\SDL_stdinc.h:503
   pragma Import (C, SDL_acos, "SDL_acos");

   function SDL_acosf (x : float) return float;  -- ..\SDL2_tmp\SDL_stdinc.h:504
   pragma Import (C, SDL_acosf, "SDL_acosf");

   function SDL_asin (x : double) return double;  -- ..\SDL2_tmp\SDL_stdinc.h:505
   pragma Import (C, SDL_asin, "SDL_asin");

   function SDL_asinf (x : float) return float;  -- ..\SDL2_tmp\SDL_stdinc.h:506
   pragma Import (C, SDL_asinf, "SDL_asinf");

   function SDL_atan (x : double) return double;  -- ..\SDL2_tmp\SDL_stdinc.h:507
   pragma Import (C, SDL_atan, "SDL_atan");

   function SDL_atanf (x : float) return float;  -- ..\SDL2_tmp\SDL_stdinc.h:508
   pragma Import (C, SDL_atanf, "SDL_atanf");

   function SDL_atan2 (x : double; y : double) return double;  -- ..\SDL2_tmp\SDL_stdinc.h:509
   pragma Import (C, SDL_atan2, "SDL_atan2");

   function SDL_atan2f (x : float; y : float) return float;  -- ..\SDL2_tmp\SDL_stdinc.h:510
   pragma Import (C, SDL_atan2f, "SDL_atan2f");

   function SDL_ceil (x : double) return double;  -- ..\SDL2_tmp\SDL_stdinc.h:511
   pragma Import (C, SDL_ceil, "SDL_ceil");

   function SDL_ceilf (x : float) return float;  -- ..\SDL2_tmp\SDL_stdinc.h:512
   pragma Import (C, SDL_ceilf, "SDL_ceilf");

   function SDL_copysign (x : double; y : double) return double;  -- ..\SDL2_tmp\SDL_stdinc.h:513
   pragma Import (C, SDL_copysign, "SDL_copysign");

   function SDL_copysignf (x : float; y : float) return float;  -- ..\SDL2_tmp\SDL_stdinc.h:514
   pragma Import (C, SDL_copysignf, "SDL_copysignf");

   function SDL_cos (x : double) return double;  -- ..\SDL2_tmp\SDL_stdinc.h:515
   pragma Import (C, SDL_cos, "SDL_cos");

   function SDL_cosf (x : float) return float;  -- ..\SDL2_tmp\SDL_stdinc.h:516
   pragma Import (C, SDL_cosf, "SDL_cosf");

   function SDL_exp (x : double) return double;  -- ..\SDL2_tmp\SDL_stdinc.h:517
   pragma Import (C, SDL_exp, "SDL_exp");

   function SDL_expf (x : float) return float;  -- ..\SDL2_tmp\SDL_stdinc.h:518
   pragma Import (C, SDL_expf, "SDL_expf");

   function SDL_fabs (x : double) return double;  -- ..\SDL2_tmp\SDL_stdinc.h:519
   pragma Import (C, SDL_fabs, "SDL_fabs");

   function SDL_fabsf (x : float) return float;  -- ..\SDL2_tmp\SDL_stdinc.h:520
   pragma Import (C, SDL_fabsf, "SDL_fabsf");

   function SDL_floor (x : double) return double;  -- ..\SDL2_tmp\SDL_stdinc.h:521
   pragma Import (C, SDL_floor, "SDL_floor");

   function SDL_floorf (x : float) return float;  -- ..\SDL2_tmp\SDL_stdinc.h:522
   pragma Import (C, SDL_floorf, "SDL_floorf");

   function SDL_fmod (x : double; y : double) return double;  -- ..\SDL2_tmp\SDL_stdinc.h:523
   pragma Import (C, SDL_fmod, "SDL_fmod");

   function SDL_fmodf (x : float; y : float) return float;  -- ..\SDL2_tmp\SDL_stdinc.h:524
   pragma Import (C, SDL_fmodf, "SDL_fmodf");

   function SDL_log (x : double) return double;  -- ..\SDL2_tmp\SDL_stdinc.h:525
   pragma Import (C, SDL_log, "SDL_log");

   function SDL_logf (x : float) return float;  -- ..\SDL2_tmp\SDL_stdinc.h:526
   pragma Import (C, SDL_logf, "SDL_logf");

   function SDL_log10 (x : double) return double;  -- ..\SDL2_tmp\SDL_stdinc.h:527
   pragma Import (C, SDL_log10, "SDL_log10");

   function SDL_log10f (x : float) return float;  -- ..\SDL2_tmp\SDL_stdinc.h:528
   pragma Import (C, SDL_log10f, "SDL_log10f");

   function SDL_pow (x : double; y : double) return double;  -- ..\SDL2_tmp\SDL_stdinc.h:529
   pragma Import (C, SDL_pow, "SDL_pow");

   function SDL_powf (x : float; y : float) return float;  -- ..\SDL2_tmp\SDL_stdinc.h:530
   pragma Import (C, SDL_powf, "SDL_powf");

   function SDL_scalbn (x : double; n : int) return double;  -- ..\SDL2_tmp\SDL_stdinc.h:531
   pragma Import (C, SDL_scalbn, "SDL_scalbn");

   function SDL_scalbnf (x : float; n : int) return float;  -- ..\SDL2_tmp\SDL_stdinc.h:532
   pragma Import (C, SDL_scalbnf, "SDL_scalbnf");

   function SDL_sin (x : double) return double;  -- ..\SDL2_tmp\SDL_stdinc.h:533
   pragma Import (C, SDL_sin, "SDL_sin");

   function SDL_sinf (x : float) return float;  -- ..\SDL2_tmp\SDL_stdinc.h:534
   pragma Import (C, SDL_sinf, "SDL_sinf");

   function SDL_sqrt (x : double) return double;  -- ..\SDL2_tmp\SDL_stdinc.h:535
   pragma Import (C, SDL_sqrt, "SDL_sqrt");

   function SDL_sqrtf (x : float) return float;  -- ..\SDL2_tmp\SDL_stdinc.h:536
   pragma Import (C, SDL_sqrtf, "SDL_sqrtf");

   function SDL_tan (x : double) return double;  -- ..\SDL2_tmp\SDL_stdinc.h:537
   pragma Import (C, SDL_tan, "SDL_tan");

   function SDL_tanf (x : float) return float;  -- ..\SDL2_tmp\SDL_stdinc.h:538
   pragma Import (C, SDL_tanf, "SDL_tanf");

  -- The SDL implementation of iconv() returns these error codes  
  -- SDL_iconv_* are now always real symbols/types, not macros or inlined.  
   type u_SDL_iconv_t is null record;   -- incomplete struct

   type SDL_iconv_t is access all u_SDL_iconv_t;  -- ..\SDL2_tmp\SDL_stdinc.h:547

   function SDL_iconv_open (tocode : Interfaces.C.Strings.chars_ptr; fromcode : Interfaces.C.Strings.chars_ptr) return SDL_iconv_t;  -- ..\SDL2_tmp\SDL_stdinc.h:548
   pragma Import (C, SDL_iconv_open, "SDL_iconv_open");

   function SDL_iconv_close (cd : SDL_iconv_t) return int;  -- ..\SDL2_tmp\SDL_stdinc.h:550
   pragma Import (C, SDL_iconv_close, "SDL_iconv_close");

   function SDL_iconv
     (cd : SDL_iconv_t;
      inbuf : System.Address;
      inbytesleft : access stddef_h.size_t;
      outbuf : System.Address;
      outbytesleft : access stddef_h.size_t) return stddef_h.size_t;  -- ..\SDL2_tmp\SDL_stdinc.h:551
   pragma Import (C, SDL_iconv, "SDL_iconv");

  --*
  -- *  This function converts a string between encodings in one pass, returning a
  -- *  string that must be freed with SDL_free() or NULL on error.
  --  

   function SDL_iconv_string
     (tocode : Interfaces.C.Strings.chars_ptr;
      fromcode : Interfaces.C.Strings.chars_ptr;
      inbuf : Interfaces.C.Strings.chars_ptr;
      inbytesleft : stddef_h.size_t) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_stdinc.h:558
   pragma Import (C, SDL_iconv_string, "SDL_iconv_string");

  -- force builds using Clang's static analysis tools to use literal C runtime
  --   here, since there are possibly tests that are ineffective otherwise.  

   function SDL_memcpy4
     (dst : System.Address;
      src : System.Address;
      dwords : stddef_h.size_t) return System.Address;  -- ..\SDL2_tmp\SDL_stdinc.h:594
   pragma Import (C, SDL_memcpy4, "SDL_memcpy4");

  -- Ends C function definitions when using C++  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_stdinc_h;
