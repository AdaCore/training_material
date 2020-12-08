pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with stddef_h;
with Interfaces.C.Strings;

package intrin_h is

   --  unsupported macro: WINAPI __stdcall
  --*
  -- * This file has no copyright assigned and is placed in the Public Domain.
  -- * This file is part of the mingw-w64 runtime package.
  -- * No warranty is given; refer to the file DISCLAIMER.PD within this package.
  --  

  -- The purpose of this file is to provide support for MSVC's intrinsics (what gcc calls
  --   Builtins) in gcc.  In MSVC, there are several features for intrinsics:
  --   - Intrinsics can either be implemented inline (via the compiler), or implemented as functions.
  --   - You can specify which approach you prefer either globally (via compile switch /Oi) or
  --     on a function by function basis via pragmas.
  --   - Before you can use any of the intrinsics, they must be declared via a prototype.  For
  --     whatever reason, MS has decided to put all the intrinsics in one file (intrin.h) AND
  --     to put duplicate copies of some of these prototypes in various platform sdk headers.
  --   In gcc, this is implemented as follows:
  --   - The inline implementations for the intrinsics are located in intrin-impl.h.  This file
  --     is included by intrin.h, as well as various platform sdk headers.
  --   - Including intrin.h will create definitions/implementations for all available MSVC intrinsics.
  --   - Including various platforms sdk headers will only include the intrinsics defined in that
  --     header.  As of this writing, only winnt.h and winbase.h use this approach.
  --   - If an application defines its own prototypes for intrinsics (ie without including any
  --     platform header or intrin.h), the symbols will be resolved from the library.  Since this
  --     will likely result in the code being invoked via 'call', performance may be degraded.
  --   If you wish to implement intrinsic functions that are defined in intrin.h but are not
  --   yet implemented in mingw-w64, see the comments at the top of intrin-impl.h.
  -- 

  -- * Intrins shiped with GCC conflict with our versions in C++, because they don't use extern "C"
  -- * linkage while our variants use them. We try to work around this by including those headers
  -- * here wrapped in extern "C" block. It's still possible that those intrins will get default
  -- * C++ linkage (when GCC headers are explicitly included before intrin.h), but at least their
  -- * guards will prevent duplicated declarations and avoid conflicts.
  -- *
  -- * On GCC 4.9 we may always include those headers. On older GCCs, we may do it only if CPU
  -- * features used by them are enabled, so we need to check macros like __SSE__ or __MMX__ first.
  --  

  -- Make sure _mm_malloc and _mm_free are defined.   
  -- NOTE: it's not included by MS version, but we do it to try work around C++/C linkage differences  
  -- Put all declarations potentially colliding with POSIX headers here.
  --	   So far, Cygwin is the only POSIX system using this header file.
  --	   If that ever changes, make sure to tweak the guarding ifndef.  

   function c_abs (u_X : int) return int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\intrin.h:242
   pragma Import (C, c_abs, "abs");

   function ceil (arg1 : double) return double;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\intrin.h:243
   pragma Import (C, ceil, "ceil");

   function labs (u_X : long) return long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\intrin.h:244
   pragma Import (C, labs, "labs");

   function memcmp
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : stddef_h.size_t) return int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\intrin.h:246
   pragma Import (C, memcmp, "memcmp");

   function memcpy
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : stddef_h.size_t) return System.Address;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\intrin.h:247
   pragma Import (C, memcpy, "memcpy");

   function memset
     (arg1 : System.Address;
      arg2 : int;
      arg3 : stddef_h.size_t) return System.Address;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\intrin.h:248
   pragma Import (C, memset, "memset");

   function strcat (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\intrin.h:249
   pragma Import (C, strcat, "strcat");

   function strcmp (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : Interfaces.C.Strings.chars_ptr) return int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\intrin.h:250
   pragma Import (C, strcmp, "strcmp");

   function strcpy (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\intrin.h:251
   pragma Import (C, strcpy, "strcpy");

   function strlen (arg1 : Interfaces.C.Strings.chars_ptr) return stddef_h.size_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\intrin.h:252
   pragma Import (C, strlen, "strlen");

   function wcscat (arg1 : access wchar_t; arg2 : access wchar_t) return access wchar_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\intrin.h:255
   pragma Import (C, wcscat, "wcscat");

   function wcscmp (arg1 : access wchar_t; arg2 : access wchar_t) return int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\intrin.h:256
   pragma Import (C, wcscmp, "wcscmp");

   function wcscpy (arg1 : access wchar_t; arg2 : access wchar_t) return access wchar_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\intrin.h:257
   pragma Import (C, wcscpy, "wcscpy");

   function wcslen (arg1 : access wchar_t) return stddef_h.size_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\intrin.h:258
   pragma Import (C, wcslen, "wcslen");

   --  skipped func _byteswap_ushort

   --  skipped func _byteswap_ulong

   --  skipped func _byteswap_uint64

   --  skipped func _disable

   --  skipped func __emul

   --  skipped func __emulu

   --  skipped func _enable

  -- __MACHINEI(__LONG32 __cdecl _InterlockedDecrement(__LONG32 volatile *)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(__MINGW_EXTENSION __int64 _InterlockedDecrement64(__int64 volatile *)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(__LONG32 _InterlockedExchange(__LONG32 volatile *,__LONG32)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(__MINGW_EXTENSION __int64 _InterlockedExchange64(__int64 volatile *,__int64)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(void *_InterlockedExchangePointer(void *volatile *,void *)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(__LONG32 _InterlockedExchangeAdd(__LONG32 volatile *,__LONG32)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(__MINGW_EXTENSION __int64 _InterlockedExchangeAdd64(__int64 volatile *,__int64)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(__LONG32 _InterlockedCompareExchange (__LONG32 volatile *,__LONG32,__LONG32)) moved to psdk_inc/intrin-impl.h  
   --  skipped func _InterlockedCompare64Exchange128

   --  skipped func _InterlockedCompare64Exchange128_acq

   --  skipped func _InterlockedCompare64Exchange128_rel

  -- __MACHINEI(__MINGW_EXTENSION __int64 _InterlockedCompareExchange64(__int64 volatile *,__int64,__int64)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(void *_InterlockedCompareExchangePointer (void *volatile *,void *,void *)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(__LONG32 __cdecl _InterlockedIncrement(__LONG32 volatile *)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(__MINGW_EXTENSION __int64 _InterlockedIncrement64(__int64 volatile *)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIW64(__LONG32 _InterlockedOr(__LONG32 volatile *,__LONG32)) moved to psdk_inc/intrin-impl.h  
   --  skipped func _InterlockedOr8

   --  skipped func _InterlockedOr16

  -- __MACHINEW64(__MINGW_EXTENSION __int64 _InterlockedOr64(__int64 volatile *,__int64)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIW64(__LONG32 _InterlockedXor(__LONG32 volatile *,__LONG32)) moved to psdk_inc/intrin-impl.h  
   --  skipped func _InterlockedXor8

   --  skipped func _InterlockedXor16

  -- __MACHINEW64(__MINGW_EXTENSION __int64 _InterlockedXor64(__int64 volatile *,__int64)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIW64(__LONG32 _InterlockedAnd(__LONG32 volatile *,__LONG32)) moved to psdk_inc/intrin-impl.h  
   --  skipped func _InterlockedAnd8

   --  skipped func _InterlockedAnd16

  -- __MACHINEW64(__MINGW_EXTENSION __int64 _InterlockedAnd64(__int64 volatile *,__int64)) moved to psdk_inc/intrin-impl.h  
   --  skipped func _inp

   function inp (arg1 : unsigned_short) return int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\intrin.h:400
   pragma Import (C, inp, "inp");

   --  skipped func _inpd

   function inpd (arg1 : unsigned_short) return unsigned_long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\intrin.h:402
   pragma Import (C, inpd, "inpd");

   --  skipped func _inpw

   function inpw (arg1 : unsigned_short) return unsigned_short;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\intrin.h:404
   pragma Import (C, inpw, "inpw");

   --  skipped func _lrotl

   --  skipped func _lrotr

   --  skipped func __ll_lshift

   --  skipped func __ll_rshift

   --  skipped func _outp

   function outp (arg1 : unsigned_short; arg2 : int) return int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\intrin.h:479
   pragma Import (C, outp, "outp");

   --  skipped func _outpd

   function outpd (arg1 : unsigned_short; arg2 : unsigned_long) return unsigned_long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\intrin.h:481
   pragma Import (C, outpd, "outpd");

   --  skipped func _outpw

   function outpw (arg1 : unsigned_short; arg2 : unsigned_short) return unsigned_short;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\intrin.h:483
   pragma Import (C, outpw, "outpw");

  -- __MACHINECE(void _ReadWriteBarrier(void)) moved to psdk_inc/intrin-impl.h  
   --  skipped func _ReturnAddress

   --  skipped func _rotl

   --  skipped func _rotr

   --  skipped func _rotl64

   --  skipped func _rotr64

   --  skipped func _setjmp

   --  skipped func _setjmpex

   --  skipped func _strset

   function strset (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : int) return Interfaces.C.Strings.chars_ptr;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\intrin.h:535
   pragma Import (C, strset, "strset");

   --  skipped func __ull_rshift

  -- __MACHINECE(void _WriteBarrier(void)) moved to psdk_inc/intrin-impl.h  
   --  skipped func _AddressOfReturnAddress

  --    __MACHINEX86X_NOIA64(void _mm_prefetch(char const*,int))  
  --		__MACHINEX86X(__m128d _mm_shuffle_pd(__m128d,__m128d,int))  
  --		__MACHINEX86X(__m128i _mm_slli_si128(__m128i,int))  
  --    __MACHINEX86X(__m128i _mm_slli_epi16(__m128i,int))  
  --    __MACHINEX86X(__m128i _mm_slli_epi32(__m128i,int))  
  --    __MACHINEX86X(__m128i _mm_slli_epi64(__m128i,int))  
  --    __MACHINEX86X(__m128i _mm_srai_epi16(__m128i,int))  
  --    __MACHINEX86X(__m128i _mm_srai_epi32(__m128i,int))  
  --		__MACHINEX86X(__m128i _mm_srli_si128(__m128i,int))  
  --    __MACHINEX86X(__m128i _mm_srli_epi16(__m128i,int))  
  --    __MACHINEX86X(__m128i _mm_srli_epi32(__m128i,int))  
  --    __MACHINEX86X(__m128i _mm_srli_epi64(__m128i,int))  
  --		__MACHINEX86X(int _mm_extract_epi16(__m128i,int))  
  --		__MACHINEX86X(__m128i _mm_insert_epi16(__m128i,int,int))  
  --		__MACHINEX86X(__m128i _mm_shuffle_epi32(__m128i,int))  
  --		__MACHINEX86X(__m128i _mm_shufflehi_epi16(__m128i,int))  
  --		__MACHINEX86X(__m128i _mm_shufflelo_epi16(__m128i,int))  
  -- __MACHINEI(void _WriteBarrier(void)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(void _ReadWriteBarrier(void)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIA64(void _WriteBarrier(void)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIA64(void _ReadWriteBarrier(void)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(void __faststorefence(void)) moved to psdk_inc/intrin-impl.h  
   --  skipped func __mulh

   --  skipped func __umulh

  -- __MACHINEX64(__MINGW_EXTENSION unsigned __int64 __readcr0(void)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(__MINGW_EXTENSION unsigned __int64 __readcr2(void)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(__MINGW_EXTENSION unsigned __int64 __readcr3(void)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(__MINGW_EXTENSION unsigned __int64 __readcr4(void)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(__MINGW_EXTENSION unsigned __int64 __readcr8(void)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIA32(unsigned __LONG32 __readcr0(void)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIA32(unsigned __LONG32 __readcr2(void)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIA32(unsigned __LONG32 __readcr3(void)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIA32(unsigned __LONG32 __readcr4(void)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIA32(unsigned __LONG32 __readcr8(void)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(__MINGW_EXTENSION void __writecr0(unsigned __int64)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(__MINGW_EXTENSION void __writecr3(unsigned __int64)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(__MINGW_EXTENSION void __writecr4(unsigned __int64)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(__MINGW_EXTENSION void __writecr8(unsigned __int64)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIA32(void __writecr0(unsigned)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIA32(void __writecr3(unsigned)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIA32(void __writecr4(unsigned)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIA32(void __writecr8(unsigned)) moved to psdk_inc/intrin-impl.h  
   --  skipped func __wbinvd

   --  skipped func __invlpg

  -- __MACHINEI(__MINGW_EXTENSION unsigned __int64 __readmsr(unsigned __LONG32)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(__MINGW_EXTENSION void __writemsr(unsigned __LONG32,unsigned __int64)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(void __movsb(unsigned char *,unsigned char const *,size_t)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(void __movsw(unsigned short *,unsigned short const *,size_t)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(void __movsd(unsigned __LONG32 *,unsigned __LONG32 const *,size_t)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(__MINGW_EXTENSION void __movsq(unsigned long long *,unsigned long long const *,size_t)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(unsigned char __readgsbyte(unsigned __LONG32 Offset)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(unsigned short __readgsword(unsigned __LONG32 Offset)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(unsigned __LONG32 __readgsdword(unsigned __LONG32 Offset)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(__MINGW_EXTENSION unsigned __int64 __readgsqword(unsigned __LONG32 Offset)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(void __writegsbyte(unsigned __LONG32 Offset,unsigned char Data)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(void __writegsword(unsigned __LONG32 Offset,unsigned short Data)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(void __writegsdword(unsigned __LONG32 Offset,unsigned __LONG32 Data)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(__MINGW_EXTENSION void __writegsqword(unsigned __LONG32 Offset,unsigned __int64 Data)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(unsigned char __inbyte(unsigned short Port)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(unsigned short __inword(unsigned short Port)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(unsigned __LONG32 __indword(unsigned short Port)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(void __outbyte(unsigned short Port,unsigned char Data)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(void __outword(unsigned short Port,unsigned short Data)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(void __outdword(unsigned short Port,unsigned __LONG32 Data)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(void __inbytestring(unsigned short Port,unsigned char *Buffer,unsigned __LONG32 Count)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(void __inwordstring(unsigned short Port,unsigned short *Buffer,unsigned __LONG32 Count)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(void __indwordstring(unsigned short Port,unsigned __LONG32 *Buffer,unsigned __LONG32 Count)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(void __outbytestring(unsigned short Port,unsigned char *Buffer,unsigned __LONG32 Count)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(void __outwordstring(unsigned short Port,unsigned short *Buffer,unsigned __LONG32 Count)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(void __outdwordstring(unsigned short Port,unsigned __LONG32 *Buffer,unsigned __LONG32 Count)) moved to psdk_inc/intrin-impl.h  
   --  skipped func __getcallerseflags

   --  skipped func _mm_stream_si64x

  -- __MACHINEI(void __stosb(unsigned char *,unsigned char,size_t)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(void __stosw(unsigned short *,unsigned short,size_t)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(void __stosd(unsigned __LONG32 *,unsigned __LONG32,size_t)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(__MINGW_EXTENSION void __stosq(unsigned __int64 *,unsigned __int64,size_t)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIW64(unsigned char _bittest(__LONG32 const *a,__LONG32 b)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIW64(unsigned char _bittestandset(__LONG32 *a,__LONG32 b)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIW64(unsigned char _bittestandreset(__LONG32 *a,__LONG32 b)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIW64(unsigned char _bittestandcomplement(__LONG32 *a,__LONG32 b)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(unsigned char InterlockedBitTestAndSet(volatile __LONG32 *a,__LONG32 b)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(unsigned char InterlockedBitTestAndReset(volatile __LONG32 *a,__LONG32 b)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(unsigned char InterlockedBitTestAndComplement(volatile __LONG32 *a,__LONG32 b)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(unsigned char _interlockedbittestandset(__LONG32 *a,__LONG32 b)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(unsigned char _interlockedbittestandreset(__LONG32 *a,__LONG32 b)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(unsigned char _interlockedbittestandcomplement(__LONG32 *a,__LONG32 b)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEW64(__MINGW_EXTENSION unsigned char _bittest64(__int64 const *a,__int64 b)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEW64(__MINGW_EXTENSION unsigned char _bittestandset64(__int64 *a,__int64 b)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEW64(__MINGW_EXTENSION unsigned char _bittestandreset64(__int64 *a,__int64 b)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEW64(__MINGW_EXTENSION unsigned char _bittestandcomplement64(__int64 *a,__int64 b)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(__MINGW_EXTENSION unsigned char InterlockedBitTestAndSet64(volatile __int64 *a,__int64 b)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(__MINGW_EXTENSION unsigned char InterlockedBitTestAndReset64(volatile __int64 *a,__int64 b)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(__MINGW_EXTENSION unsigned char InterlockedBitTestAndComplement64(volatile __int64 *a,__int64 b)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(__MINGW_EXTENSION unsigned char _interlockedbittestandset64(__int64 *a,__int64 b)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(__MINGW_EXTENSION unsigned char _interlockedbittestandreset64(__int64 *a,__int64 b)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEX64(__MINGW_EXTENSION unsigned char _interlockedbittestandcomplement64(__int64 *a,__int64 b)) moved to psdk_inc/intrin-impl.h  
   --  skipped func __readpmc

   --  skipped func __segmentlimit

  -- __MACHINEIA32(unsigned char __readfsbyte(unsigned __LONG32 Offset)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIA32(unsigned short __readfsword(unsigned __LONG32 Offset)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIA32(unsigned __LONG32 __readfsdword(unsigned __LONG32 Offset)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIA32(__MINGW_EXTENSION unsigned __int64 __readfsqword(unsigned __LONG32 Offset)) intrinsic doesn't actually exist  
  -- __MACHINEIA32(void __writefsbyte(unsigned __LONG32 Offset,unsigned char Data)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIA32(void __writefsword(unsigned __LONG32 Offset,unsigned short Data)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIA32(void __writefsdword(unsigned __LONG32 Offset,unsigned __LONG32 Data)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIA32(__MINGW_EXTENSION void __writefsqword(unsigned __LONG32 Offset,unsigned __int64 Data)) intrinsic doesn't actually exist  
  -- __MACHINEIW64(unsigned char _BitScanForward(unsigned __LONG32 *Index,unsigned __LONG32 Mask)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIW64(unsigned char _BitScanReverse(unsigned __LONG32 *Index,unsigned __LONG32 Mask)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEW64(__MINGW_EXTENSION unsigned char _BitScanForward64(unsigned __LONG32 *Index,unsigned __int64 Mask)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEW64(__MINGW_EXTENSION unsigned char _BitScanReverse64(unsigned __LONG32 *Index,unsigned __int64 Mask)) moved to psdk_inc/intrin-impl.h  
   --  skipped func _wcsset

  -- __MACHINEW64(__MINGW_EXTENSION unsigned __int64 __shiftleft128(unsigned __int64 LowPart,unsigned __int64 HighPart,unsigned char Shift)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEW64(__MINGW_EXTENSION unsigned __int64 __shiftright128(unsigned __int64 LowPart,unsigned __int64 HighPart,unsigned char Shift)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEW64(__MINGW_EXTENSION unsigned __int64 _umul128(unsigned __int64 multiplier,unsigned __int64 multiplicand,unsigned __int64 *highproduct)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEW64(__MINGW_EXTENSION __int64 _mul128(__int64 multiplier,__int64 multiplicand,__int64 *highproduct)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEI(void __int2c(void)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIW64(void _ReadBarrier(void)) moved to psdk_inc/intrin-impl.h  
   --  skipped func _rotr8

   --  skipped func _rotr16

   --  skipped func _rotl8

   --  skipped func _rotl16

  -- __MACHINEIW64(short _InterlockedIncrement16(short volatile *Addend)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIW64(short _InterlockedDecrement16(short volatile *Addend)) moved to psdk_inc/intrin-impl.h  
  -- __MACHINEIW64(short _InterlockedCompareExchange16(short volatile *Destination,short Exchange,short Comparand)) moved to psdk_inc/intrin-impl.h  
   --  skipped func __nvreg_save_fence

   --  skipped func __nvreg_restore_fence

   --  skipped func _InterlockedCompareExchange16_np

   --  skipped func _InterlockedCompareExchange_np

   --  skipped func _InterlockedCompareExchange64_np

   --  skipped func _InterlockedCompareExchangePointer_np

   --  skipped func _InterlockedCompare64Exchange128_np

   --  skipped func _InterlockedCompare64Exchange128_acq_np

   --  skipped func _InterlockedCompare64Exchange128_rel_np

   --  skipped func _InterlockedAnd_np

   --  skipped func _InterlockedAnd8_np

   --  skipped func _InterlockedAnd16_np

   --  skipped func _InterlockedAnd64_np

   --  skipped func _InterlockedOr_np

   --  skipped func _InterlockedOr8_np

   --  skipped func _InterlockedOr16_np

   --  skipped func _InterlockedOr64_np

   --  skipped func _InterlockedXor_np

   --  skipped func _InterlockedXor8_np

   --  skipped func _InterlockedXor16_np

   --  skipped func _InterlockedXor64_np

end intrin_h;
