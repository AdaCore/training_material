pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with umingw_h;
with Interfaces.C.Strings;
with System;
with Interfaces.C.Extensions;

package stdlib_h is


   EXIT_SUCCESS : constant := 0;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:30
   EXIT_FAILURE : constant := 1;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:31
   --  unsupported macro: onexit_t _onexit_t

   RAND_MAX : constant := 16#7fff#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:92
   --  unsupported macro: MB_CUR_MAX ___mb_cur_max_func()
   --  unsupported macro: errno (*_errno())
   --  unsupported macro: strtod __strtod
   --  unsupported macro: sys_errlist _sys_errlist
   --  unsupported macro: sys_nerr _sys_nerr
   --  unsupported macro: environ _environ

   type u_onexit_t is access function return int;
   pragma Convention (C, u_onexit_t);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:36

   type u_div_t is record
      quot : aliased int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:47
      c_rem : aliased int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:48
   end record;
   pragma Convention (C_Pass_By_Copy, u_div_t);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:46

   subtype div_t is u_div_t;

   type u_ldiv_t is record
      quot : aliased long;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:52
      c_rem : aliased long;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:53
   end record;
   pragma Convention (C_Pass_By_Copy, u_ldiv_t);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:51

   subtype ldiv_t is u_ldiv_t;

   type u_LDOUBLE_ld_array is array (0 .. 9) of aliased unsigned_char;
   type u_LDOUBLE is record
      ld : aliased u_LDOUBLE_ld_array;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:62
   end record;
   pragma Convention (C_Pass_By_Copy, u_LDOUBLE);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:63

   --  skipped anonymous struct anon_1

   type u_CRT_DOUBLE is record
      x : aliased double;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:69
   end record;
   pragma Convention (C_Pass_By_Copy, u_CRT_DOUBLE);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:70

   --  skipped anonymous struct anon_2

   type u_CRT_FLOAT is record
      f : aliased float;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:73
   end record;
   pragma Convention (C_Pass_By_Copy, u_CRT_FLOAT);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:74

   --  skipped anonymous struct anon_3

   type u_LONGDOUBLE is record
      x : aliased long_double;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:80
   end record;
   pragma Convention (C_Pass_By_Copy, u_LONGDOUBLE);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:81

   --  skipped anonymous struct anon_4

   --  skipped anonymous struct anon_5

   type u_LDBL12_ld12_array is array (0 .. 11) of aliased unsigned_char;
   type u_LDBL12 is record
      ld12 : aliased u_LDBL12_ld12_array;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:87
   end record;
   pragma Convention (C_Pass_By_Copy, u_LDBL12);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:88

   type u_purecall_handler is access procedure;
   pragma Convention (C, u_purecall_handler);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:127

   --  skipped func _set_purecall_handler

   --  skipped func _get_purecall_handler

   type u_invalid_parameter_handler is access procedure
        (arg1 : access wchar_t;
         arg2 : access wchar_t;
         arg3 : access wchar_t;
         arg4 : unsigned;
         arg5 : umingw_h.uintptr_t);
   pragma Convention (C, u_invalid_parameter_handler);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:132

   --  skipped func _set_invalid_parameter_handler

   --  skipped func _get_invalid_parameter_handler

   --  skipped func _errno

   --  skipped func _set_errno

   --  skipped func _get_errno

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

   procedure c_exit (u_Code : int);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:302
   pragma Import (C, c_exit, "exit");

   --  skipped func _exit

   --  skipped func _Exit

   procedure c_abort;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:316
   pragma Import (C, c_abort, "abort");

   --  skipped func _set_abort_behavior

   function c_abs (u_X : int) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:325
   pragma Import (C, c_abs, "abs");

   function labs (u_X : long) return long;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:326
   pragma Import (C, labs, "labs");

   --  skipped func _abs64

   function atexit (arg1 : access procedure) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:330
   pragma Import (C, atexit, "atexit");

   function atof (u_String : Interfaces.C.Strings.chars_ptr) return double;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:333
   pragma Import (C, atof, "atof");

   --  skipped func _atof_l

   function atoi (u_Str : Interfaces.C.Strings.chars_ptr) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:336
   pragma Import (C, atoi, "atoi");

   --  skipped func _atoi_l

   function atol (u_Str : Interfaces.C.Strings.chars_ptr) return long;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:338
   pragma Import (C, atol, "atol");

   --  skipped func _atol_l

   function bsearch
     (u_Key : System.Address;
      u_Base : System.Address;
      u_NumOfElements : umingw_h.size_t;
      u_SizeOfElements : umingw_h.size_t;
      u_PtFuncCompare : access function (arg1 : System.Address; arg2 : System.Address) return int) return System.Address;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:342
   pragma Import (C, bsearch, "bsearch");

   procedure qsort
     (u_Base : System.Address;
      u_NumOfElements : umingw_h.size_t;
      u_SizeOfElements : umingw_h.size_t;
      u_PtFuncCompare : access function (arg1 : System.Address; arg2 : System.Address) return int);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:343
   pragma Import (C, qsort, "qsort");

   --  skipped func _byteswap_ushort

   --  skipped func _byteswap_uint64

   function div (u_Numerator : int; u_Denominator : int) return div_t;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:348
   pragma Import (C, div, "div");

   function getenv (u_VarName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:349
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

   function ldiv (u_Numerator : long; u_Denominator : long) return ldiv_t;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:359
   pragma Import (C, ldiv, "ldiv");

   --  skipped func _ltoa

   function mblen (u_Ch : Interfaces.C.Strings.chars_ptr; u_MaxCount : umingw_h.size_t) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:361
   pragma Import (C, mblen, "mblen");

   --  skipped func _mblen_l

   --  skipped func _mbstrlen

   --  skipped func _mbstrlen_l

   --  skipped func _mbstrnlen

   --  skipped func _mbstrnlen_l

   function mbtowc
     (u_DstCh : access wchar_t;
      u_SrcCh : Interfaces.C.Strings.chars_ptr;
      u_SrcSizeInBytes : umingw_h.size_t) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:367
   pragma Import (C, mbtowc, "mbtowc");

   --  skipped func _mbtowc_l

   function mbstowcs
     (u_Dest : access wchar_t;
      u_Source : Interfaces.C.Strings.chars_ptr;
      u_MaxCount : umingw_h.size_t) return umingw_h.size_t;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:369
   pragma Import (C, mbstowcs, "mbstowcs");

   --  skipped func _mbstowcs_l

   function rand return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:371
   pragma Import (C, rand, "rand");

   --  skipped func _set_error_mode

   procedure srand (u_Seed : unsigned);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:373
   pragma Import (C, srand, "srand");

   function strtof (nptr : Interfaces.C.Strings.chars_ptr; endptr : System.Address) return float;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:378
   pragma Import (C, strtof, "strtof");

   function strtold (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : System.Address) return long_double;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:379
   pragma Import (C, strtold, "strtold");

   function strtod (uu_nptr : Interfaces.C.Strings.chars_ptr; uu_endptr : System.Address) return double;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:386
   pragma Import (C, strtod, "strtod");

   --  skipped func _strtod_l

   function strtol
     (u_Str : Interfaces.C.Strings.chars_ptr;
      u_EndPtr : System.Address;
      u_Radix : int) return long;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:399
   pragma Import (C, strtol, "strtol");

   --  skipped func _strtol_l

   function strtoul
     (u_Str : Interfaces.C.Strings.chars_ptr;
      u_EndPtr : System.Address;
      u_Radix : int) return unsigned_long;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:401
   pragma Import (C, strtoul, "strtoul");

   --  skipped func _strtoul_l

   function c_system (u_Command : Interfaces.C.Strings.chars_ptr) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:405
   pragma Import (C, c_system, "system");

   --  skipped func _ultoa

   function wctomb (u_MbCh : Interfaces.C.Strings.chars_ptr; u_WCh : wchar_t) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:408
   pragma Import (C, wctomb, "wctomb");

   --  skipped func _wctomb_l

   function wcstombs
     (u_Dest : Interfaces.C.Strings.chars_ptr;
      u_Source : access wchar_t;
      u_MaxCount : umingw_h.size_t) return umingw_h.size_t;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:410
   pragma Import (C, wcstombs, "wcstombs");

   --  skipped func _wcstombs_l

   function calloc (u_NumOfElements : umingw_h.size_t; u_SizeOfElements : umingw_h.size_t) return System.Address;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:415
   pragma Import (C, calloc, "calloc");

   procedure free (u_Memory : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:416
   pragma Import (C, free, "free");

   function malloc (u_Size : umingw_h.size_t) return System.Address;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:417
   pragma Import (C, malloc, "malloc");

   function realloc (u_Memory : System.Address; u_NewSize : umingw_h.size_t) return System.Address;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:418
   pragma Import (C, realloc, "realloc");

   --  skipped func _recalloc

   --  skipped func _aligned_free

   --  skipped func _aligned_malloc

   --  skipped func _aligned_offset_malloc

   --  skipped func _aligned_realloc

   --  skipped func _aligned_recalloc

   --  skipped func _aligned_offset_realloc

   --  skipped func _aligned_offset_recalloc

   --  skipped func _itow

   --  skipped func _ltow

   --  skipped func _ultow

   function wcstod (u_Str : access wchar_t; u_EndPtr : System.Address) return double;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:444
   pragma Import (C, wcstod, "wcstod");

   function wcstof (nptr : access wchar_t; endptr : System.Address) return float;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:452
   pragma Import (C, wcstof, "wcstof");

   function wcstold (arg1 : access wchar_t; arg2 : System.Address) return long_double;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:453
   pragma Import (C, wcstold, "wcstold");

   --  skipped func _wcstod_l

   function wcstol
     (u_Str : access wchar_t;
      u_EndPtr : System.Address;
      u_Radix : int) return long;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:456
   pragma Import (C, wcstol, "wcstol");

   --  skipped func _wcstol_l

   function wcstoul
     (u_Str : access wchar_t;
      u_EndPtr : System.Address;
      u_Radix : int) return unsigned_long;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:458
   pragma Import (C, wcstoul, "wcstoul");

   --  skipped func _wcstoul_l

   --  skipped func _wgetenv

   --  skipped func _wsystem

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

   --  skipped func _lrotl

   --  skipped func _lrotr

   --  skipped func _makepath

   --  skipped func _onexit

   --  skipped func _putenv

   --  skipped func _rotl64

   --  skipped func _rotr

   --  skipped func _rotl

   --  skipped func _rotr64

   --  skipped func _searchenv

   --  skipped func _splitpath

   --  skipped func _swab

   --  skipped func _wfullpath

   --  skipped func _wmakepath

   --  skipped func _wputenv

   --  skipped func _wsearchenv

   --  skipped func _wsplitpath

   --  skipped func _beep

   --  skipped func _seterrormode

   --  skipped func _sleep

   function ecvt
     (u_Val : double;
      u_NumOfDigits : int;
      u_PtDec : access int;
      u_PtSign : access int) return Interfaces.C.Strings.chars_ptr;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:574
   pragma Import (C, ecvt, "ecvt");

   function fcvt
     (u_Val : double;
      u_NumOfDec : int;
      u_PtDec : access int;
      u_PtSign : access int) return Interfaces.C.Strings.chars_ptr;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:575
   pragma Import (C, fcvt, "fcvt");

   function gcvt
     (u_Val : double;
      u_NumOfDigits : int;
      u_DstBuf : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:576
   pragma Import (C, gcvt, "gcvt");

   function itoa
     (u_Val : int;
      u_DstBuf : Interfaces.C.Strings.chars_ptr;
      u_Radix : int) return Interfaces.C.Strings.chars_ptr;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:577
   pragma Import (C, itoa, "itoa");

   function ltoa
     (u_Val : long;
      u_DstBuf : Interfaces.C.Strings.chars_ptr;
      u_Radix : int) return Interfaces.C.Strings.chars_ptr;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:578
   pragma Import (C, ltoa, "ltoa");

   function putenv (u_EnvString : Interfaces.C.Strings.chars_ptr) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:579
   pragma Import (C, putenv, "putenv");

   procedure swab
     (u_Buf1 : Interfaces.C.Strings.chars_ptr;
      u_Buf2 : Interfaces.C.Strings.chars_ptr;
      u_SizeInBytes : int);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:583
   pragma Import (C, swab, "swab");

   function ultoa
     (u_Val : unsigned_long;
      u_Dstbuf : Interfaces.C.Strings.chars_ptr;
      u_Radix : int) return Interfaces.C.Strings.chars_ptr;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:586
   pragma Import (C, ultoa, "ultoa");

   function onexit (u_Func : u_onexit_t) return u_onexit_t;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:587
   pragma Import (C, onexit, "onexit");

   --  skipped anonymous struct anon_6

   type lldiv_t is record
      quot : aliased Long_Long_Integer;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:593
      c_rem : aliased Long_Long_Integer;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:593
   end record;
   pragma Convention (C_Pass_By_Copy, lldiv_t);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:593

   function lldiv (arg1 : Long_Long_Integer; arg2 : Long_Long_Integer) return lldiv_t;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:595
   pragma Import (C, lldiv, "lldiv");

   function llabs (arg1 : Long_Long_Integer) return Long_Long_Integer;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:597
   pragma Import (C, llabs, "llabs");

   function strtoll
     (arg1 : Interfaces.C.Strings.chars_ptr;
      arg2 : System.Address;
      arg3 : int) return Long_Long_Integer;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:602
   pragma Import (C, strtoll, "strtoll");

   function strtoull
     (arg1 : Interfaces.C.Strings.chars_ptr;
      arg2 : System.Address;
      arg3 : int) return Extensions.unsigned_long_long;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:603
   pragma Import (C, strtoull, "strtoull");

   function atoll (arg1 : Interfaces.C.Strings.chars_ptr) return Long_Long_Integer;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:606
   pragma Import (C, atoll, "atoll");

   function wtoll (arg1 : access wchar_t) return Long_Long_Integer;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:609
   pragma Import (C, wtoll, "wtoll");

   function lltoa
     (arg1 : Long_Long_Integer;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : int) return Interfaces.C.Strings.chars_ptr;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:610
   pragma Import (C, lltoa, "lltoa");

   function ulltoa
     (arg1 : Extensions.unsigned_long_long;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : int) return Interfaces.C.Strings.chars_ptr;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:611
   pragma Import (C, ulltoa, "ulltoa");

   function lltow
     (arg1 : Long_Long_Integer;
      arg2 : access wchar_t;
      arg3 : int) return access wchar_t;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:612
   pragma Import (C, lltow, "lltow");

   function ulltow
     (arg1 : Extensions.unsigned_long_long;
      arg2 : access wchar_t;
      arg3 : int) return access wchar_t;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdlib.h:613
   pragma Import (C, ulltow, "ulltow");

end stdlib_h;
