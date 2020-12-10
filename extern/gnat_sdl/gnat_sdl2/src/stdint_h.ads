pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;

package stdint_h is

   INT8_MIN : constant := (-128);  --  d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:76
   INT16_MIN : constant := (-32768);  --  d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:77
   INT32_MIN : constant := (-2147483647 - 1);  --  d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:78
   INT64_MIN : constant := (-9223372036854775807 - 1);  --  d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:79

   INT8_MAX : constant := 127;  --  d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:81
   INT16_MAX : constant := 32767;  --  d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:82
   INT32_MAX : constant := 2147483647;  --  d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:83
   INT64_MAX : constant := 9223372036854775807;  --  d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:84

   UINT8_MAX : constant := 255;  --  d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:86
   UINT16_MAX : constant := 65535;  --  d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:87
   UINT32_MAX : constant := 16#ffffffff#;  --  d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:88
   UINT64_MAX : constant := 16#ffffffffffffffff#;  --  d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:89
   --  unsupported macro: INT_LEAST8_MIN INT8_MIN
   --  unsupported macro: INT_LEAST16_MIN INT16_MIN
   --  unsupported macro: INT_LEAST32_MIN INT32_MIN
   --  unsupported macro: INT_LEAST64_MIN INT64_MIN
   --  unsupported macro: INT_LEAST8_MAX INT8_MAX
   --  unsupported macro: INT_LEAST16_MAX INT16_MAX
   --  unsupported macro: INT_LEAST32_MAX INT32_MAX
   --  unsupported macro: INT_LEAST64_MAX INT64_MAX
   --  unsupported macro: UINT_LEAST8_MAX UINT8_MAX
   --  unsupported macro: UINT_LEAST16_MAX UINT16_MAX
   --  unsupported macro: UINT_LEAST32_MAX UINT32_MAX
   --  unsupported macro: UINT_LEAST64_MAX UINT64_MAX
   --  unsupported macro: INT_FAST8_MIN INT8_MIN
   --  unsupported macro: INT_FAST16_MIN INT16_MIN
   --  unsupported macro: INT_FAST32_MIN INT32_MIN
   --  unsupported macro: INT_FAST64_MIN INT64_MIN
   --  unsupported macro: INT_FAST8_MAX INT8_MAX
   --  unsupported macro: INT_FAST16_MAX INT16_MAX
   --  unsupported macro: INT_FAST32_MAX INT32_MAX
   --  unsupported macro: INT_FAST64_MAX INT64_MAX
   --  unsupported macro: UINT_FAST8_MAX UINT8_MAX
   --  unsupported macro: UINT_FAST16_MAX UINT16_MAX
   --  unsupported macro: UINT_FAST32_MAX UINT32_MAX
   --  unsupported macro: UINT_FAST64_MAX UINT64_MAX
   --  unsupported macro: INTPTR_MIN INT64_MIN
   --  unsupported macro: INTPTR_MAX INT64_MAX
   --  unsupported macro: UINTPTR_MAX UINT64_MAX
   --  unsupported macro: INTMAX_MIN INT64_MIN
   --  unsupported macro: INTMAX_MAX INT64_MAX
   --  unsupported macro: UINTMAX_MAX UINT64_MAX
   --  unsupported macro: PTRDIFF_MIN INT64_MIN
   --  unsupported macro: PTRDIFF_MAX INT64_MAX
   --  unsupported macro: SIG_ATOMIC_MIN INT32_MIN
   --  unsupported macro: SIG_ATOMIC_MAX INT32_MAX
   --  unsupported macro: SIZE_MAX UINT64_MAX

   WCHAR_MIN : constant := 0;  --  d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:161
   WCHAR_MAX : constant := 16#ffff#;  --  d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:162

   WINT_MIN : constant := 0;  --  d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:168
   WINT_MAX : constant := 16#ffff#;  --  d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:169
   --  arg-macro: function INT8_C (val)
   --    return INT_LEAST8_MAX-INT_LEAST8_MAX+(val);
   --  arg-macro: function INT16_C (val)
   --    return INT_LEAST16_MAX-INT_LEAST16_MAX+(val);
   --  arg-macro: function INT32_C (val)
   --    return INT_LEAST32_MAX-INT_LEAST32_MAX+(val);
   --  unsupported macro: INT64_C(val) val ##LL
   --  arg-macro: function UINT8_C (val)
   --    return val;
   --  arg-macro: function UINT16_C (val)
   --    return val;
   --  unsupported macro: UINT32_C(val) (val ##U)
   --  unsupported macro: UINT64_C(val) val ##ULL
   --  unsupported macro: INTMAX_C(val) val ##LL
   --  unsupported macro: UINTMAX_C(val) val ##ULL

  --*
  -- * This file has no copyright assigned and is placed in the Public Domain.
  -- * This file is part of the mingw-w64 runtime package.
  -- * No warranty is given; refer to the file DISCLAIMER.PD within this package.
  --  

  -- ISO C9x  7.18  Integer types <stdint.h>
  -- * Based on ISO/IEC SC22/WG14 9899 Committee draft (SC22 N2794)
  -- *
  -- *  THIS SOFTWARE IS NOT COPYRIGHTED
  -- *
  -- *  Contributor: Danny Smith <danny_r_smith_2001@yahoo.co.nz>
  -- *
  -- *  This source code is offered for use in the public domain. You may
  -- *  use, modify or distribute it freely.
  -- *
  -- *  This code is distributed in the hope that it will be useful but
  -- *  WITHOUT ANY WARRANTY. ALL WARRANTIES, EXPRESS OR IMPLIED ARE HEREBY
  -- *  DISCLAIMED. This includes but is not limited to warranties of
  -- *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  -- *
  -- *  Date: 2000-12-02
  --  

  -- 7.18.1.1  Exact-width integer types  
   subtype int8_t is signed_char;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:35

   subtype uint8_t is unsigned_char;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:36

   subtype int16_t is short;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:37

   subtype uint16_t is unsigned_short;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:38

   subtype int32_t is int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:39

   subtype uint32_t is unsigned;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:40

   subtype int64_t is Long_Long_Integer;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:41

   subtype uint64_t is Extensions.unsigned_long_long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:42

  -- 7.18.1.2  Minimum-width integer types  
   subtype int_least8_t is signed_char;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:45

   subtype uint_least8_t is unsigned_char;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:46

   subtype int_least16_t is short;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:47

   subtype uint_least16_t is unsigned_short;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:48

   subtype int_least32_t is int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:49

   subtype uint_least32_t is unsigned;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:50

   subtype int_least64_t is Long_Long_Integer;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:51

   subtype uint_least64_t is Extensions.unsigned_long_long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:52

  --  7.18.1.3  Fastest minimum-width integer types
  -- *  Not actually guaranteed to be fastest for all purposes
  -- *  Here we use the exact-width types for 8 and 16-bit ints.
  --  

   subtype int_fast8_t is signed_char;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:58

   subtype uint_fast8_t is unsigned_char;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:59

   subtype int_fast16_t is short;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:60

   subtype uint_fast16_t is unsigned_short;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:61

   subtype int_fast32_t is int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:62

   subtype uint_fast32_t is unsigned;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:63

   subtype int_fast64_t is Long_Long_Integer;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:64

   subtype uint_fast64_t is Extensions.unsigned_long_long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:65

  -- 7.18.1.5  Greatest-width integer types  
   subtype intmax_t is Long_Long_Integer;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:68

   subtype uintmax_t is Extensions.unsigned_long_long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdint.h:69

  -- 7.18.2  Limits of specified-width integer types  
  -- 7.18.2.1  Limits of exact-width integer types  
  -- 7.18.2.2  Limits of minimum-width integer types  
  -- 7.18.2.3  Limits of fastest minimum-width integer types  
  -- 7.18.2.4  Limits of integer types capable of holding
  --    object pointers  

  -- 7.18.2.5  Limits of greatest-width integer types  
  -- 7.18.3  Limits of other integer types  
  -- * wint_t is unsigned short for compatibility with MS runtime
  --  

  -- 7.18.4  Macros for integer constants  
  -- 7.18.4.1  Macros for minimum-width integer constants
  --    Accoding to Douglas Gwyn <gwyn@arl.mil>:
  --	"This spec was changed in ISO/IEC 9899:1999 TC1; in ISO/IEC
  --	9899:1999 as initially published, the expansion was required
  --	to be an integer constant of precisely matching type, which
  --	is impossible to accomplish for the shorter types on most
  --	platforms, because C99 provides no standard way to designate
  --	an integer constant with width less than that of type int.
  --	TC1 changed this to require just an integer constant
  --	*expression* with *promoted* type."
  --	The trick used here is from Clive D W Feather.
  -- 

  --  The 'trick' doesn't work in C89 for long long because, without
  --    suffix, (val) will be evaluated as int, not intmax_t  

  -- 7.18.4.2  Macros for greatest-width integer constants  
end stdint_h;
