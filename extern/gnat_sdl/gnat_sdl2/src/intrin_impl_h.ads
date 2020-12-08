pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package intrin_impl_h is

  --*
  -- * This file has no copyright assigned and is placed in the Public Domain.
  -- * This file is part of the mingw-w64 runtime package.
  -- * No warranty is given; refer to the file DISCLAIMER.PD within this package.
  --  

  -- There are 3 separate ways this file is intended to be used:
  --   1) Included from intrin.h.  In this case, all intrinsics in this file get declarations and
  --      implementations.  No special #defines are needed for this case.
  --   2) Included from the library versions of these functions (ie mingw-w64-crt\intrincs\*.c).  All
  --      intrinsics in this file must also be included in the library.  In this case, only the 
  --      specific functions requested will get defined, and they will not be defined as inline.  If
  --      you have followed the instructions (below) for adding functions to this file, then all you 
  --      need to have in the .c file is the following:
  --      #define __INTRINSIC_ONLYSPECIAL
  --      #define __INTRINSIC_SPECIAL___stosb // Causes code generation in intrin-impl.h
  --      #include <intrin.h>
  --   3) Included from various platform sdk headers.  Some platform sdk headers (such as winnt.h)
  --      define a subset of intrinsics.  To avoid potential conflicts, this file is designed to
  --      allow for specific subsets of functions to be defined.  This is done by defining the 
  --      appropriate variable before including this file:
  --      #define __INTRINSIC_GROUP_WINNT
  --      #include <psdk_inc/intrin-impl.h>
  --   In all cases, it is acceptable to include this file multiple times in any order (ie include 
  --   winnt.h to get its subset, then include intrin.h to get everything, or vice versa).
  --   See also the comments at the top of intrin.h.
  -- 

  -- To add an implementation for a new intrinsic to this file, you should comment out the current prototype in intrin.h.
  --   If the function you are adding is not in intrin.h, you should not be adding it to this file.  This file is only
  --   for MSVC intrinsics.
  --   Make sure you put your definition in the right section (x86 vs x64), and use this outline when adding definitions 
  --   to this file:
  --#if __INTRINSIC_PROLOG(__int2c)
  --<prototype goes here>
  --__INTRINSICS_USEINLINE 
  --<code goes here>
  --#define __INTRINSIC_DEFINED___int2c
  --#endif
  -- 

  -- Note that there is no file-wide #if to prevent intrin-impl.h from being
  --   included multiple times.  This is because this file might be included multiple
  --   times to define various subsets of the functions it contains.  

  -- However we do check for __MINGW_INTRIN_INLINE.  In theory this means we
  --   can work with other compilers.   

  -- These macros are used by the routines below.  While this file may be included 
  --   multiple times, these macros only need to be defined once.  

  -- This macro is used by __stosb, __stosw, __stosd, __stosq  
  -- Parameters: (FunctionName, DataType, Operator)
  --   FunctionName: Any valid function name
  --   DataType: BYTE, WORD, DWORD or DWORD64
  --   InstructionSizeIntel: b, w, d, q (not b,w,l,q)  

  -- While we don't need the output values for Dest or Count, we
  --   must still inform the compiler the asm changes them.  

  -- This macro is used by InterlockedAnd, InterlockedOr, InterlockedXor, InterlockedAnd64, InterlockedOr64, InterlockedXor64  
  -- Parameters: (FunctionName, DataType, Operator)
  --   FunctionName: Any valid function name
  --   DataType: __LONG32 or __int64
  --   Operator: One of xor, or, and  

  -- This macro is used by InterlockedBitTestAndSet, InterlockedBitTestAndReset, InterlockedBitTestAndComplement,
  --   InterlockedBitTestAndSet64, InterlockedBitTestAndReset64, InterlockedBitTestAndComplement64
  --   _interlockedbittestandset, _interlockedbittestandreset, _interlockedbittestandcomplement
  --   _interlockedbittestandset64, _interlockedbittestandreset64, _interlockedbittestandcomplement64  

  -- Parameters: (FunctionName, DataType, AsmCode, OffsetConstraint, Volatile)
  --   FunctionName: Any valid function name
  --   DataType: __LONG32 or __int64
  --   OffsetConstraint: either "I" for 32bit data types or "J" for 64.
  --   Volatile: either volatile or blank.  

  -- This macro is used by YieldProcessor when compiling x86 w/o SSE2.
  --It generates the same opcodes as _mm_pause.   

  -- This macro is used by DbgRaiseAssertionFailure and __int2c
  --Parameters: (IntNum)
  --IntNum: Interrupt number in hex  

  -- This macro is used by MemoryBarrier when compiling x86 w/o SSE2. 
  --Note that on i386, xchg performs an implicit lock.  

  -- This macro is used by __readfsbyte, __readfsword, __readfsdword
  --                         __readgsbyte, __readgsword, __readgsdword, __readgsqword
  --Parameters: (FunctionName, DataType, Segment)
  --   FunctionName: Any valid function name
  --   DataType: char, short, __LONG32 or __int64
  --   Segment: fs or gs  

  -- This macro is used by __writefsbyte, __writefsword, __writefsdword
  --                         __writegsbyte, __writegsword, __writegsdword, __writegsqword
  --Parameters: (FunctionName, DataType, Segment)
  --   FunctionName: Any valid function name
  --   DataType: char, short, __LONG32 or __int64
  --   Segment: fs or gs  

  -- This macro is used by _BitScanForward, _BitScanForward64, _BitScanReverse _BitScanReverse64
  --Parameters: (FunctionName, DataType, Segment)
  --   FunctionName: Any valid function name
  --   DataType: unsigned __LONG32 or unsigned __int64
  --   Statement: BSF or BSR  

  -- This macro is used by _bittest & _bittest64
  --Parameters: (FunctionName, DataType, OffsetConstraint)
  --   FunctionName: Any valid function name
  --   DataType: __LONG32 or __int64
  --   OffsetConstraint: either "I" for 32bit data types or "J" for 64.
  --    

  -- This macro is used by _bittestandset, _bittestandreset, _bittestandcomplement,
  --   _bittestandset64, _bittestandreset64, _bittestandcomplement64
  --Parameters: (FunctionName, DataType, Statement, OffsetConstraint)
  --   FunctionName: Any valid function name
  --   DataType: __LONG32 or __int64
  --   Statement: asm statement (bts, btr, btc)
  --   OffsetConstraint: either "I" for 32bit data types or "J" for 64.
  --    

  -- This macro is used by __inbyte, __inword, __indword
  --Parameters: (FunctionName, DataType)
  --   FunctionName: Any valid function name
  --   DataType: unsigned char, unsigned short, unsigned __LONG32
  --    

  -- This macro is used by __outbyte, __outword, __outdword
  --Parameters: (FunctionName, DataType)
  --   FunctionName: Any valid function name
  --   DataType: unsigned char, unsigned short, unsigned __LONG32
  --    

  -- This macro is used by __inbytestring, __inwordstring, __indwordstring
  --Parameters: (FunctionName, DataType, InstructionSizeAtt, InstructionSizeIntel)
  --   FunctionName: Any valid function name
  --   DataType: unsigned char, unsigned short, unsigned __LONG32
  --   InstructionSizeAtt: b, w, l
  --   InstructionSizeIntel: b, w, d (not b,w,l)
  --    

  -- This macro is used by __outbytestring, __outwordstring, __outdwordstring
  --Parameters: (FunctionName, DataType, InstructionSizeAtt, InstructionSizeIntel)
  --   FunctionName: Any valid function name
  --   DataType: unsigned char, unsigned short, unsigned __LONG32
  --   InstructionSizeAtt: b, w, l
  --   InstructionSizeIntel: b, w, d (not b,w,l)
  --    

  -- This macro is used by __readcr0, __readcr2, __readcr3, __readcr4, __readcr8
  --Parameters: (FunctionName, DataType, RegisterNumber)
  --   FunctionName: Any valid function name
  --   DataType: unsigned __LONG32, unsigned __int64
  --   RegisterNumber: 0, 2, 3, 4, 8
  --    

  -- This macro is used by __writecr0, __writecr2, __writecr3, __writecr4, __writecr8
  --Parameters: (FunctionName, DataType, RegisterNumber)
  --   FunctionName: Any valid function name
  --   DataType: unsigned __LONG32, unsigned __int64
  --   RegisterNumber: 0, 2, 3, 4, 8
  --    

  -- This macro is used by __movsb, __movsd, __movsq, __movsw
  --Parameters: (FunctionName, DataType, RegisterNumber)
  --   FunctionName: Any valid function name
  --   DataType: unsigned char, unsigned short, unsigned __LONG32, unsigned __int64
  --   InstructionSize: b, w, d, q
  --    

  -- The Barrier functions can never be in the library.  Since gcc only
  --supports ReadWriteBarrier, map all 3 to do the same.  

  -- The logic for this macro is:
  --   if the function is not yet defined AND
  --   (
  --       (if we are not just defining special OR 
  --           (we are defining special AND this is one of the ones we are defining)
  --       )
  --   )
  -- 

  -- Normally __INTRINSIC_ONLYSPECIAL is used to indicate that we are
  --   being included in the library version of the intrinsic (case 2).  However,
  --   that really only affects the definition of __INTRINSICS_USEINLINE.
  --   So here we are letting it serve an additional purpose of only defining
  --   the intrinsics for a certain file (case 3).  For example, to create the
  --   intrinsics for the functions in winnt.h, define __INTRINSIC_GROUP_WINNT.
  --   Note that this file can be included multiple times, and as a result
  --   there can be overlap (definitions that appear in more than one
  --   file).  This is handled by __INTRINSIC_DEFINED_*
  --   If no groups are defined (such as what happens when including intrin.h),
  --   all intrinsics are defined.    

  -- If __INTRINSIC_ONLYSPECIAL is defined at this point, we are processing case 2.  In 
  --   that case, don't go looking for groups  

  -- Note that this gets undefined at the end of this file  
  -- Note that this gets undefined at the end of this file  
  -- To add an additional group, put the #ifdef and definitions here.  
   --  skipped func __faststorefence

  -- Turns out this is actually faster than MS's "trick" on newer cpus.  Note
  --    that this builtin performs an implicit ReadWriteBarrier.  

   --  skipped func __stosq

  -- unused param  
   --  skipped func _interlockedbittestandset64

  -- unused param  
   --  skipped func _interlockedbittestandreset64

  -- unused param  
   --  skipped func _interlockedbittestandcomplement64

   function InterlockedBitTestAndSet64 (Base : access Long_Long_Integer; Offset : Long_Long_Integer) return unsigned_char;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\psdk_inc\intrin-impl.h:526
   pragma Import (C, InterlockedBitTestAndSet64, "InterlockedBitTestAndSet64");

   function InterlockedBitTestAndReset64 (Base : access Long_Long_Integer; Offset : Long_Long_Integer) return unsigned_char;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\psdk_inc\intrin-impl.h:533
   pragma Import (C, InterlockedBitTestAndReset64, "InterlockedBitTestAndReset64");

   function InterlockedBitTestAndComplement64 (Base : access Long_Long_Integer; Offset : Long_Long_Integer) return unsigned_char;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\psdk_inc\intrin-impl.h:540
   pragma Import (C, InterlockedBitTestAndComplement64, "InterlockedBitTestAndComplement64");

   --  skipped func _InterlockedAnd64

   --  skipped func _InterlockedOr64

   --  skipped func _InterlockedXor64

   --  skipped func _InterlockedIncrement64

   --  skipped func _InterlockedDecrement64

   --  skipped func _InterlockedExchange64

   --  skipped func _InterlockedExchangeAdd64

   --  skipped func __readgsbyte

   --  skipped func __readgsword

   --  skipped func __readgsdword

   --  skipped func __readgsqword

   --  skipped func __writegsbyte

   --  skipped func __writegsword

   --  skipped func __writegsdword

   --  skipped func __writegsqword

   --  skipped func _BitScanForward64

   --  skipped func _BitScanReverse64

   --  skipped func _bittest64

   --  skipped func _bittestandset64

   --  skipped func _bittestandreset64

   --  skipped func _bittestandcomplement64

   --  skipped func __readcr0

   --  skipped func __readcr2

   --  skipped func __readcr3

   --  skipped func __readcr4

   --  skipped func __readcr8

   --  skipped func __writecr0

   --  skipped func __writecr3

   --  skipped func __writecr4

   --  skipped func __writecr8

   --  skipped func __movsq

   --  skipped func _umul128

   --  skipped func _mul128

   --  skipped func __shiftleft128

   --  skipped func __shiftright128

  -- *****************************************************  
   --  skipped func __int2c

   --  skipped func __stosb

   --  skipped func __stosw

   --  skipped func __stosd

  -- unused param  
   --  skipped func _interlockedbittestandset

  -- unused param  
   --  skipped func _interlockedbittestandreset

  -- unused param  
   --  skipped func _interlockedbittestandcomplement

   function InterlockedBitTestAndSet (Base : access long; Offset : long) return unsigned_char;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\psdk_inc\intrin-impl.h:891
   pragma Import (C, InterlockedBitTestAndSet, "InterlockedBitTestAndSet");

   function InterlockedBitTestAndReset (Base : access long; Offset : long) return unsigned_char;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\psdk_inc\intrin-impl.h:898
   pragma Import (C, InterlockedBitTestAndReset, "InterlockedBitTestAndReset");

   function InterlockedBitTestAndComplement (Base : access long; Offset : long) return unsigned_char;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\psdk_inc\intrin-impl.h:905
   pragma Import (C, InterlockedBitTestAndComplement, "InterlockedBitTestAndComplement");

   --  skipped func _InterlockedAnd

   --  skipped func _InterlockedOr

   --  skipped func _InterlockedXor

   --  skipped func _InterlockedIncrement16

   --  skipped func _InterlockedDecrement16

   --  skipped func _InterlockedCompareExchange16

   --  skipped func _InterlockedExchangeAdd

   --  skipped func _InterlockedCompareExchange

   --  skipped func _InterlockedIncrement

   --  skipped func _InterlockedDecrement

   --  skipped func _InterlockedExchange

   --  skipped func _InterlockedCompareExchange64

   --  skipped func _InterlockedCompareExchangePointer

   --  skipped func _InterlockedExchangePointer

   --  skipped func _BitScanForward

   --  skipped func _BitScanReverse

   --  skipped func _bittest

   --  skipped func _bittestandset

   --  skipped func _bittestandreset

   --  skipped func _bittestandcomplement

   --  skipped func __inbyte

   --  skipped func __inword

   --  skipped func __indword

   --  skipped func __outbyte

   --  skipped func __outword

   --  skipped func __outdword

   --  skipped func __inbytestring

   --  skipped func __inwordstring

   --  skipped func __indwordstring

   --  skipped func __outbytestring

   --  skipped func __outwordstring

   --  skipped func __outdwordstring

   --  skipped func __cpuid

   --  skipped func __readmsr

   --  skipped func __writemsr

   --  skipped func __movsb

   --  skipped func __movsw

   --  skipped func __movsd

  -- *****************************************************  
end intrin_impl_h;
