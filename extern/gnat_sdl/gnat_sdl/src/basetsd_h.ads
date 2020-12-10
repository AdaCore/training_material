pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with Interfaces.C.Extensions;
with System;

package basetsd_h is

   --  unsupported macro: SPOINTER_32 POINTER_SIGNED POINTER_32
   --  unsupported macro: UPOINTER_32 POINTER_UNSIGNED POINTER_32

   ADDRESS_TAG_BIT : constant := 16#80000000#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:97
   --  arg-macro: function HandleToULong (h)
   --    return (ULONG)(ULONG_PTR)(h);
   --  arg-macro: function HandleToLong (h)
   --    return (LONG)(LONG_PTR) (h);
   --  arg-macro: function ULongToHandle (ul)
   --    return (HANDLE)(ULONG_PTR) (ul);
   --  arg-macro: function LongToHandle (h)
   --    return (HANDLE)(LONG_PTR) (h);
   --  arg-macro: function PtrToUlong (p)
   --    return (ULONG)(ULONG_PTR) (p);
   --  arg-macro: function PtrToLong (p)
   --    return (LONG)(LONG_PTR) (p);
   --  arg-macro: function PtrToUint (p)
   --    return (UINT)(UINT_PTR) (p);
   --  arg-macro: function PtrToInt (p)
   --    return (INT)(INT_PTR) (p);
   --  arg-macro: function PtrToUshort (p)
   --    return (unsigned short)(ULONG_PTR)(p);
   --  arg-macro: function PtrToShort (p)
   --    return (short)(LONG_PTR)(p);
   --  arg-macro: function IntToPtr (i)
   --    return (VOID *)(INT_PTR)((int)i);
   --  arg-macro: function UIntToPtr (ui)
   --    return (VOID *)(UINT_PTR)((unsigned int)ui);
   --  arg-macro: function LongToPtr (l)
   --    return (VOID *)(LONG_PTR)((long)l);
   --  arg-macro: function ULongToPtr (ul)
   --    return (VOID *)(ULONG_PTR)((unsigned long)ul);
   --  arg-macro: function Ptr32ToPtr (p)
   --    return (void *) (ULONG_PTR) p;
   --  arg-macro: function Handle32ToHandle (h)
   --    return Ptr32ToPtr(h);
   --  arg-macro: function PtrToPtr32 (p)
   --    return (void *) (ULONG_PTR) p;
   --  arg-macro: function HandleToHandle32 (h)
   --    return PtrToPtr32(h);
   --  arg-macro: procedure HandleToUlong (h)
   --    HandleToULong(h)
   --  arg-macro: procedure UlongToHandle (ul)
   --    ULongToHandle(ul)
   --  arg-macro: procedure UlongToPtr (ul)
   --    ULongToPtr(ul)
   --  arg-macro: procedure UintToPtr (ui)
   --    UIntToPtr(ui)
   --  unsupported macro: MAXUINT_PTR (~((UINT_PTR)0))
   --  unsupported macro: MAXINT_PTR ((INT_PTR)(MAXUINT_PTR >> 1))
   --  unsupported macro: MININT_PTR (~MAXINT_PTR)
   --  unsupported macro: MAXULONG_PTR (~((ULONG_PTR)0))
   --  unsupported macro: MAXLONG_PTR ((LONG_PTR)(MAXULONG_PTR >> 1))
   --  unsupported macro: MINLONG_PTR (~MAXLONG_PTR)
   --  unsupported macro: MAXUHALF_PTR ((UHALF_PTR)~0)
   --  unsupported macro: MAXHALF_PTR ((HALF_PTR)(MAXUHALF_PTR >> 1))
   --  unsupported macro: MINHALF_PTR (~MAXHALF_PTR)

   subtype POINTER_64_INT is unsigned_long;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:14

   subtype INT8 is char;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:30

   type PINT8 is new Interfaces.C.Strings.chars_ptr;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:30

   subtype INT16 is short;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:31

   type PINT16 is access all short;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:31

   subtype INT32 is int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:32

   type PINT32 is access all int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:32

   subtype INT64 is Long_Long_Integer;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:33

   type PINT64 is access all Long_Long_Integer;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:33

   subtype UINT8 is unsigned_char;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:34

   type PUINT8 is access all unsigned_char;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:34

   subtype UINT16 is unsigned_short;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:35

   type PUINT16 is access all unsigned_short;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:35

   subtype UINT32 is unsigned;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:36

   type PUINT32 is access all unsigned;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:36

   subtype UINT64 is Extensions.unsigned_long_long;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:37

   type PUINT64 is access all Extensions.unsigned_long_long;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:37

   subtype LONG32 is int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:38

   type PLONG32 is access all int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:38

   subtype ULONG32 is unsigned;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:39

   type PULONG32 is access all unsigned;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:39

   subtype DWORD32 is unsigned;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:40

   type PDWORD32 is access all unsigned;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:40

   subtype INT_PTR is int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:53

   type PINT_PTR is access all int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:53

   subtype UINT_PTR is unsigned;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:54

   type PUINT_PTR is access all unsigned;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:54

   subtype LONG_PTR is long;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:55

   type PLONG_PTR is access all long;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:55

   subtype ULONG_PTR is unsigned_long;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:56

   type PULONG_PTR is access all unsigned_long;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:56

   subtype UHALF_PTR is unsigned_short;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:99

   type PUHALF_PTR is access all unsigned_short;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:99

   subtype HALF_PTR is short;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:100

   type PHALF_PTR is access all short;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:100

   subtype SHANDLE_PTR is long;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:101

   subtype HANDLE_PTR is unsigned_long;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:102

   function PtrToPtr64 (p : System.Address) return System.Address;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:120
   pragma Import (C, PtrToPtr64, "PtrToPtr64");

   function Ptr64ToPtr (p : System.Address) return System.Address;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:121
   pragma Import (C, Ptr64ToPtr, "Ptr64ToPtr");

   function HandleToHandle64 (h : System.Address) return System.Address;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:122
   pragma Import (C, HandleToHandle64, "HandleToHandle64");

   function Handle64ToHandle (h : System.Address) return System.Address;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:123
   pragma Import (C, Handle64ToHandle, "Handle64ToHandle");

   subtype SIZE_T is ULONG_PTR;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:149

   type PSIZE_T is access all ULONG_PTR;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:149

   subtype SSIZE_T is LONG_PTR;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:150

   type PSSIZE_T is access all LONG_PTR;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:150

   subtype DWORD_PTR is ULONG_PTR;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:151

   type PDWORD_PTR is access all ULONG_PTR;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:151

   subtype LONG64 is Long_Long_Integer;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:152

   type PLONG64 is access all Long_Long_Integer;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:152

   subtype ULONG64 is Extensions.unsigned_long_long;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:153

   type PULONG64 is access all Extensions.unsigned_long_long;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:153

   subtype DWORD64 is Extensions.unsigned_long_long;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:154

   type PDWORD64 is access all Extensions.unsigned_long_long;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:154

   subtype KAFFINITY is ULONG_PTR;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:155

   type PKAFFINITY is access all KAFFINITY;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/basetsd.h:156

end basetsd_h;
