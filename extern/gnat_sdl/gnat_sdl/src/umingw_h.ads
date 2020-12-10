pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package umingw_h is


   USE_u_u_UUIDOF : constant := 0;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/_mingw.h:83

   WINVER : constant := 16#0502#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/_mingw.h:248

   subtype size_t is unsigned;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/_mingw.h:398

   subtype ssize_t is int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/_mingw.h:408

   subtype intptr_t is int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/_mingw.h:420

   subtype uintptr_t is unsigned;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/_mingw.h:433

   subtype ptrdiff_t is int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/_mingw.h:446

   subtype wint_t is unsigned_short;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/_mingw.h:462

   subtype wctype_t is unsigned_short;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/_mingw.h:463

   subtype errno_t is int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/_mingw.h:482

   subtype uu_time32_t is long;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/_mingw.h:487

   subtype uu_time64_t is Long_Long_Integer;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/_mingw.h:492

   subtype time_t is uu_time32_t;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/_mingw.h:498

end umingw_h;
