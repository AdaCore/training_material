pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with umingw_h;
with Interfaces.C.Strings;
with System;
with stddef_h;
with Interfaces.C.Extensions;

package stdlib_h is

   EXIT_SUCCESS : constant := 0;  --  d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:34
   EXIT_FAILURE : constant := 1;  --  d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:35
   --  unsupported macro: onexit_t _onexit_t

   RAND_MAX : constant := 16#7fff#;  --  d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:96
   --  unsupported macro: MB_CUR_MAX ___mb_cur_max_func()
   --  unsupported macro: errno (*_errno())
   --  unsupported macro: sys_errlist _sys_errlist
   --  unsupported macro: sys_nerr _sys_nerr
   --  unsupported macro: environ _environ

  --*
  -- * This file has no copyright assigned and is placed in the Public Domain.
  -- * This file is part of the mingw-w64 runtime package.
  -- * No warranty is given; refer to the file DISCLAIMER.PD within this package.
  --  

   type u_onexit_t is access function return int;
   pragma Convention (C, u_onexit_t);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:40

   type u_div_t is record
      quot : aliased int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:51
      c_rem : aliased int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:52
   end record;
   pragma Convention (C_Pass_By_Copy, u_div_t);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:50

   subtype div_t is u_div_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:53

   type u_ldiv_t is record
      quot : aliased long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:56
      c_rem : aliased long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:57
   end record;
   pragma Convention (C_Pass_By_Copy, u_ldiv_t);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:55

   subtype ldiv_t is u_ldiv_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:58

   --  skipped anonymous struct anon_17

   type u_LDOUBLE_ld_array is array (0 .. 9) of aliased unsigned_char;
   type u_LDOUBLE is record
      ld : aliased u_LDOUBLE_ld_array;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:66
   end record;
   pragma Convention (C_Pass_By_Copy, u_LDOUBLE);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:67

   --  skipped anonymous struct anon_18

   type u_CRT_DOUBLE is record
      x : aliased double;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:73
   end record;
   pragma Convention (C_Pass_By_Copy, u_CRT_DOUBLE);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:74

   --  skipped anonymous struct anon_19

   type u_CRT_FLOAT is record
      f : aliased float;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:77
   end record;
   pragma Convention (C_Pass_By_Copy, u_CRT_FLOAT);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:78

   type u_LONGDOUBLE is record
      x : aliased long_double;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:84
   end record;
   pragma Convention (C_Pass_By_Copy, u_LONGDOUBLE);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:85

   --  skipped anonymous struct anon_20

   --  skipped anonymous struct anon_21

   type u_LDBL12_ld12_array is array (0 .. 11) of aliased unsigned_char;
   type u_LDBL12 is record
      ld12 : aliased u_LDBL12_ld12_array;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:91
   end record;
   pragma Convention (C_Pass_By_Copy, u_LDBL12);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:92

   type u_purecall_handler is access procedure;
   pragma Convention (C, u_purecall_handler);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:131

   --  skipped func _set_purecall_handler

   --  skipped func _get_purecall_handler

   type u_invalid_parameter_handler is access procedure
        (arg1 : access wchar_t;
         arg2 : access wchar_t;
         arg3 : access wchar_t;
         arg4 : unsigned;
         arg5 : umingw_h.uintptr_t);
   pragma Convention (C, u_invalid_parameter_handler);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:136

   --  skipped func _set_invalid_parameter_handler

   --  skipped func _get_invalid_parameter_handler

   --  skipped func _errno

   --  skipped func _set_errno

   --  skipped func _get_errno

   --  skipped func __doserrno

   --  skipped func _set_doserrno

   --  skipped func _get_doserrno

   --  skipped func _get_pgmptr

   --  skipped func _get_wpgmptr

   --  skipped func _set_fmode

   --  skipped func _get_fmode

   --  skipped func _get_osplatform

   --  skipped func _get_osver

   --  skipped func _get_winver

   --  skipped func _get_winmajor

   --  skipped func _get_winminor

  -- C99 function name  
   --  skipped func _set_abort_behavior

   --  skipped func _abs64

   function atexit (arg1 : access procedure) return int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:340
   pragma Import (C, atexit, "atexit");

   function atof (u_String : Interfaces.C.Strings.chars_ptr) return double;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:343
   pragma Import (C, atof, "atof");

   --  skipped func _atof_l

   function atoi (u_Str : Interfaces.C.Strings.chars_ptr) return int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:346
   pragma Import (C, atoi, "atoi");

   --  skipped func _atoi_l

   function atol (u_Str : Interfaces.C.Strings.chars_ptr) return long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:348
   pragma Import (C, atol, "atol");

   --  skipped func _atol_l

   function bsearch
     (u_Key : System.Address;
      u_Base : System.Address;
      u_NumOfElements : stddef_h.size_t;
      u_SizeOfElements : stddef_h.size_t;
      u_PtFuncCompare : access function (arg1 : System.Address; arg2 : System.Address) return int) return System.Address;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:352
   pragma Import (C, bsearch, "bsearch");

   procedure qsort
     (u_Base : System.Address;
      u_NumOfElements : stddef_h.size_t;
      u_SizeOfElements : stddef_h.size_t;
      u_PtFuncCompare : access function (arg1 : System.Address; arg2 : System.Address) return int);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:353
   pragma Import (C, qsort, "qsort");

   function div (u_Numerator : int; u_Denominator : int) return div_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:358
   pragma Import (C, div, "div");

   function getenv (u_VarName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:359
   pragma Import (C, getenv, "getenv");

   --  skipped func _itoa

   --  skipped func _i64toa

   --  skipped func _ui64toa

   --  skipped func _atoi64

   --  skipped func _atoi64_l

   --  skipped func _strtoi64

   --  skipped func _strtoi64_l

   --  skipped func _strtoui64

   --  skipped func _strtoui64_l

   function ldiv (u_Numerator : long; u_Denominator : long) return ldiv_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:369
   pragma Import (C, ldiv, "ldiv");

   --  skipped func _ltoa

   function mblen (u_Ch : Interfaces.C.Strings.chars_ptr; u_MaxCount : stddef_h.size_t) return int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:371
   pragma Import (C, mblen, "mblen");

   --  skipped func _mblen_l

   --  skipped func _mbstrlen

   --  skipped func _mbstrlen_l

   --  skipped func _mbstrnlen

   --  skipped func _mbstrnlen_l

   function mbtowc
     (u_DstCh : access wchar_t;
      u_SrcCh : Interfaces.C.Strings.chars_ptr;
      u_SrcSizeInBytes : stddef_h.size_t) return int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:377
   pragma Import (C, mbtowc, "mbtowc");

   --  skipped func _mbtowc_l

   function mbstowcs
     (u_Dest : access wchar_t;
      u_Source : Interfaces.C.Strings.chars_ptr;
      u_MaxCount : stddef_h.size_t) return stddef_h.size_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:379
   pragma Import (C, mbstowcs, "mbstowcs");

   --  skipped func _mbstowcs_l

   function rand return int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:381
   pragma Import (C, rand, "rand");

   --  skipped func _set_error_mode

   procedure srand (u_Seed : unsigned);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:383
   pragma Import (C, srand, "srand");

  -- strtold is already an alias to __mingw_strtold  
   function strtod (u_Str : Interfaces.C.Strings.chars_ptr; u_EndPtr : System.Address) return double;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:404
   pragma Import (C, strtod, "strtod");

   function strtof (nptr : Interfaces.C.Strings.chars_ptr; endptr : System.Address) return float;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:405
   pragma Import (C, strtof, "strtof");

   function strtold (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : System.Address) return long_double;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:407
   pragma Import (C, strtold, "strtold");

  -- libmingwex.a provides a c99-compliant strtod() exported as __strtod()  
   --  skipped func __strtod

   --  skipped func __mingw_strtof

   --  skipped func __mingw_strtod

   --  skipped func __mingw_strtold

   --  skipped func _strtod_l

   function strtol
     (u_Str : Interfaces.C.Strings.chars_ptr;
      u_EndPtr : System.Address;
      u_Radix : int) return long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:421
   pragma Import (C, strtol, "strtol");

   --  skipped func _strtol_l

   function strtoul
     (u_Str : Interfaces.C.Strings.chars_ptr;
      u_EndPtr : System.Address;
      u_Radix : int) return unsigned_long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:423
   pragma Import (C, strtoul, "strtoul");

   --  skipped func _strtoul_l

   --  skipped func _ultoa

   function wctomb (u_MbCh : Interfaces.C.Strings.chars_ptr; u_WCh : wchar_t) return int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:430
   pragma Import (C, wctomb, "wctomb");

   --  skipped func _wctomb_l

   function wcstombs
     (u_Dest : Interfaces.C.Strings.chars_ptr;
      u_Source : access wchar_t;
      u_MaxCount : stddef_h.size_t) return stddef_h.size_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:432
   pragma Import (C, wcstombs, "wcstombs");

   --  skipped func _wcstombs_l

   function calloc (u_NumOfElements : stddef_h.size_t; u_SizeOfElements : stddef_h.size_t) return System.Address;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:437
   pragma Import (C, calloc, "calloc");

   procedure free (u_Memory : System.Address);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:438
   pragma Import (C, free, "free");

   function malloc (u_Size : stddef_h.size_t) return System.Address;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:439
   pragma Import (C, malloc, "malloc");

   function realloc (u_Memory : System.Address; u_NewSize : stddef_h.size_t) return System.Address;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:440
   pragma Import (C, realloc, "realloc");

   --  skipped func _recalloc

  -- Make sure that X86intrin.h doesn't produce here collisions.   
   --  skipped func _aligned_offset_malloc

   --  skipped func _aligned_realloc

   --  skipped func _aligned_recalloc

   --  skipped func _aligned_offset_realloc

   --  skipped func _aligned_offset_recalloc

   --  skipped func _itow

   --  skipped func _ltow

   --  skipped func _ultow

   --  skipped func __mingw_wcstod

   --  skipped func __mingw_wcstof

   --  skipped func __mingw_wcstold

  -- wcstold is already a mingw implementation  
   function wcstod (u_Str : access wchar_t; u_EndPtr : System.Address) return double;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:482
   pragma Import (C, wcstod, "wcstod");

   function wcstof (nptr : access wchar_t; endptr : System.Address) return float;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:483
   pragma Import (C, wcstof, "wcstof");

   function wcstold (arg1 : access wchar_t; arg2 : System.Address) return long_double;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:486
   pragma Import (C, wcstold, "wcstold");

   --  skipped func _wcstod_l

   function wcstol
     (u_Str : access wchar_t;
      u_EndPtr : System.Address;
      u_Radix : int) return long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:489
   pragma Import (C, wcstol, "wcstol");

   --  skipped func _wcstol_l

   function wcstoul
     (u_Str : access wchar_t;
      u_EndPtr : System.Address;
      u_Radix : int) return unsigned_long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:491
   pragma Import (C, wcstoul, "wcstoul");

   --  skipped func _wcstoul_l

   --  skipped func _wgetenv

   --  skipped func _wtof

   --  skipped func _wtof_l

   --  skipped func _wtoi

   --  skipped func _wtoi_l

   --  skipped func _wtol

   --  skipped func _wtol_l

   --  skipped func _i64tow

   --  skipped func _ui64tow

   --  skipped func _wtoi64

   --  skipped func _wtoi64_l

   --  skipped func _wcstoi64

   --  skipped func _wcstoi64_l

   --  skipped func _wcstoui64

   --  skipped func _wcstoui64_l

   --  skipped func _fullpath

   --  skipped func _ecvt

   --  skipped func _fcvt

   --  skipped func _gcvt

   --  skipped func _atodbl

   --  skipped func _atoldbl

   --  skipped func _atoflt

   --  skipped func _atodbl_l

   --  skipped func _atoldbl_l

   --  skipped func _atoflt_l

   --  skipped func _makepath

   --  skipped func _onexit

   procedure perror (u_ErrMsg : Interfaces.C.Strings.chars_ptr);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:546
   pragma Import (C, perror, "perror");

   --  skipped func _putenv

   --  skipped func _searchenv

   --  skipped func _splitpath

   --  skipped func _swab

   --  skipped func _wfullpath

   --  skipped func _wmakepath

   --  skipped func _wperror

   --  skipped func _wputenv

   --  skipped func _wsearchenv

   --  skipped func _wsplitpath

   --  skipped func _beep

  -- Not to be confused with  _set_error_mode (int).   
   --  skipped func _seterrormode

   --  skipped func _sleep

   function ecvt
     (u_Val : double;
      u_NumOfDigits : int;
      u_PtDec : access int;
      u_PtSign : access int) return Interfaces.C.Strings.chars_ptr;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:607
   pragma Import (C, ecvt, "ecvt");

   function fcvt
     (u_Val : double;
      u_NumOfDec : int;
      u_PtDec : access int;
      u_PtSign : access int) return Interfaces.C.Strings.chars_ptr;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:608
   pragma Import (C, fcvt, "fcvt");

   function gcvt
     (u_Val : double;
      u_NumOfDigits : int;
      u_DstBuf : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:609
   pragma Import (C, gcvt, "gcvt");

   function itoa
     (u_Val : int;
      u_DstBuf : Interfaces.C.Strings.chars_ptr;
      u_Radix : int) return Interfaces.C.Strings.chars_ptr;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:610
   pragma Import (C, itoa, "itoa");

   function ltoa
     (u_Val : long;
      u_DstBuf : Interfaces.C.Strings.chars_ptr;
      u_Radix : int) return Interfaces.C.Strings.chars_ptr;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:611
   pragma Import (C, ltoa, "ltoa");

   function putenv (u_EnvString : Interfaces.C.Strings.chars_ptr) return int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:612
   pragma Import (C, putenv, "putenv");

   procedure swab
     (u_Buf1 : Interfaces.C.Strings.chars_ptr;
      u_Buf2 : Interfaces.C.Strings.chars_ptr;
      u_SizeInBytes : int);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:616
   pragma Import (C, swab, "swab");

   function ultoa
     (u_Val : unsigned_long;
      u_Dstbuf : Interfaces.C.Strings.chars_ptr;
      u_Radix : int) return Interfaces.C.Strings.chars_ptr;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:619
   pragma Import (C, ultoa, "ultoa");

   function onexit (u_Func : u_onexit_t) return u_onexit_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:620
   pragma Import (C, onexit, "onexit");

   type lldiv_t is record
      quot : aliased Long_Long_Integer;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:626
      c_rem : aliased Long_Long_Integer;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:626
   end record;
   pragma Convention (C_Pass_By_Copy, lldiv_t);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:626

   --  skipped anonymous struct anon_22

   function lldiv (arg1 : Long_Long_Integer; arg2 : Long_Long_Integer) return lldiv_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:628
   pragma Import (C, lldiv, "lldiv");

   function llabs (arg1 : Long_Long_Integer) return Long_Long_Integer;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:630
   pragma Import (C, llabs, "llabs");

   function strtoll
     (arg1 : Interfaces.C.Strings.chars_ptr;
      arg2 : System.Address;
      arg3 : int) return Long_Long_Integer;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:635
   pragma Import (C, strtoll, "strtoll");

   function strtoull
     (arg1 : Interfaces.C.Strings.chars_ptr;
      arg2 : System.Address;
      arg3 : int) return Extensions.unsigned_long_long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:636
   pragma Import (C, strtoull, "strtoull");

  -- these are stubs for MS _i64 versions  
   function atoll (arg1 : Interfaces.C.Strings.chars_ptr) return Long_Long_Integer;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:639
   pragma Import (C, atoll, "atoll");

   function wtoll (arg1 : access wchar_t) return Long_Long_Integer;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:642
   pragma Import (C, wtoll, "wtoll");

   function lltoa
     (arg1 : Long_Long_Integer;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : int) return Interfaces.C.Strings.chars_ptr;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:643
   pragma Import (C, lltoa, "lltoa");

   function ulltoa
     (arg1 : Extensions.unsigned_long_long;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : int) return Interfaces.C.Strings.chars_ptr;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:644
   pragma Import (C, ulltoa, "ulltoa");

   function lltow
     (arg1 : Long_Long_Integer;
      arg2 : access wchar_t;
      arg3 : int) return access wchar_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:645
   pragma Import (C, lltow, "lltow");

   function ulltow
     (arg1 : Extensions.unsigned_long_long;
      arg2 : access wchar_t;
      arg3 : int) return access wchar_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\stdlib.h:646
   pragma Import (C, ulltow, "ulltow");

  -- __CRT_INLINE using non-ansi functions  
end stdlib_h;
