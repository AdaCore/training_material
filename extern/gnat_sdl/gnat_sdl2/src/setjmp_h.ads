pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with System;

package setjmp_h is

   --  arg-macro: procedure setjmp (BUF)
   --    _setjmp((BUF), __builtin_frame_address (0))
  --*
  -- * This file has no copyright assigned and is placed in the Public Domain.
  -- * This file is part of the mingw-w64 runtime package.
  -- * No warranty is given; refer to the file DISCLAIMER.PD within this package.
  --  

   type u_SETJMP_FLOAT128_Part_array is array (0 .. 1) of aliased Extensions.unsigned_long_long;
   type u_SETJMP_FLOAT128 is record
      Part : aliased u_SETJMP_FLOAT128_Part_array;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:113
   end record;
   pragma Convention (C_Pass_By_Copy, u_SETJMP_FLOAT128);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:112

   subtype SETJMP_FLOAT128 is u_SETJMP_FLOAT128;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:114

   subtype u_JBTYPE is SETJMP_FLOAT128;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:117

   type u_JUMP_BUFFER is record
      Frame : aliased Extensions.unsigned_long_long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:120
      Rbx : aliased Extensions.unsigned_long_long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:121
      Rsp : aliased Extensions.unsigned_long_long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:122
      Rbp : aliased Extensions.unsigned_long_long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:123
      Rsi : aliased Extensions.unsigned_long_long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:124
      Rdi : aliased Extensions.unsigned_long_long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:125
      R12 : aliased Extensions.unsigned_long_long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:126
      R13 : aliased Extensions.unsigned_long_long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:127
      R14 : aliased Extensions.unsigned_long_long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:128
      R15 : aliased Extensions.unsigned_long_long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:129
      Rip : aliased Extensions.unsigned_long_long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:130
      Spare : aliased Extensions.unsigned_long_long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:131
      Xmm6 : aliased SETJMP_FLOAT128;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:132
      Xmm7 : aliased SETJMP_FLOAT128;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:133
      Xmm8 : aliased SETJMP_FLOAT128;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:134
      Xmm9 : aliased SETJMP_FLOAT128;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:135
      Xmm10 : aliased SETJMP_FLOAT128;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:136
      Xmm11 : aliased SETJMP_FLOAT128;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:137
      Xmm12 : aliased SETJMP_FLOAT128;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:138
      Xmm13 : aliased SETJMP_FLOAT128;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:139
      Xmm14 : aliased SETJMP_FLOAT128;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:140
      Xmm15 : aliased SETJMP_FLOAT128;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:141
   end record;
   pragma Convention (C_Pass_By_Copy, u_JUMP_BUFFER);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:119

   type jmp_buf is array (0 .. 15) of aliased u_JBTYPE;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:147

   function mingw_getsp return System.Address;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:151
   pragma Import (C, mingw_getsp, "mingw_getsp");

   --  skipped func _setjmp3

  -- throw(...) 
   procedure ms_longjmp (u_Buf : access u_JBTYPE; u_Value : int);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:192
   pragma Import (C, ms_longjmp, "ms_longjmp");

   procedure longjmp (u_Buf : access u_JBTYPE; u_Value : int);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\setjmp.h:193
   pragma Import (C, longjmp, "longjmp");

end setjmp_h;
