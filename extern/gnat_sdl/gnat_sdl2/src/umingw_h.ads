pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;

package umingw_h is

   USE_u_u_UUIDOF : constant := 0;  --  d:\install\gpl2018\x86_64-pc-mingw32\include\_mingw.h:79

   WINVER : constant := 16#0502#;  --  d:\install\gpl2018\x86_64-pc-mingw32\include\_mingw.h:225
   --  unsupported macro: UNALIGNED __unaligned

  --*
  -- * This file has no copyright assigned and is placed in the Public Domain.
  -- * This file is part of the mingw-w64 runtime package.
  -- * No warranty is given; refer to the file DISCLAIMER.PD within this package.
  --  

  -- Include _cygwin.h if we're building a Cygwin application.  
  -- Target specific macro replacement for type "long".  In the Windows API,
  --   the type long is always 32 bit, even if the target is 64 bit (LLP64).
  --   On 64 bit Cygwin, the type long is 64 bit (LP64).  So, to get the right
  --   sized definitions and declarations, all usage of type long in the Windows
  --   headers have to be replaced by the below defined macro __LONG32.  

  -- C/C++ specific language defines.   
  -- Note the extern. This is needed to work around GCC's
  --limitations in handling dllimport attribute.   

  -- Attribute `nonnull' was valid as of gcc 3.3.  We don't use GCC's
  --   variadiac macro facility, because variadic macros cause syntax
  --   errors with  --traditional-cpp.   

  --  High byte is the major version, low byte is the minor.  
  -- other headers depend on this include  
  -- We have to define _DLL for gcc based mingw version. This define is set
  --   by VC, when DLL-based runtime is used. So, gcc based runtime just have
  --   DLL-base runtime, therefore this define has to be set.
  --   As our headers are possibly used by windows compiler having a static
  --   C-runtime, we make this definition gnu compiler specific here.   

   subtype ssize_t is Long_Long_Integer;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\_mingw.h:387

   subtype intptr_t is Long_Long_Integer;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\_mingw.h:399

   subtype uintptr_t is Extensions.unsigned_long_long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\_mingw.h:412

   subtype wint_t is unsigned_short;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\_mingw.h:443

   subtype wctype_t is unsigned_short;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\_mingw.h:444

   subtype errno_t is int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\_mingw.h:463

   subtype uu_time32_t is long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\_mingw.h:468

   subtype uu_time64_t is Long_Long_Integer;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\_mingw.h:473

   subtype time_t is uu_time64_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\_mingw.h:481

  -- MSVC defines _NATIVE_NULLPTR_SUPPORTED when nullptr is supported. We emulate it here for GCC.  
  -- We are activating __USE_MINGW_ANSI_STDIO for various define indicators.
  --   Note that we enable it also for _GNU_SOURCE in C++, but not for C case.  

  -- Enable __USE_MINGW_ANSI_STDIO if _POSIX defined
  -- * and If user did _not_ specify it explicitly...  

  -- _dowildcard is an int that controls the globbing of the command line.
  -- * The MinGW32 (mingw.org) runtime calls it _CRT_glob, so we are adding
  -- * a compatibility definition here:  you can use either of _CRT_glob or
  -- * _dowildcard .
  -- * If _dowildcard is non-zero, the command line will be globbed:  *.*
  -- * will be expanded to be all files in the startup directory.
  -- * In the mingw-w64 library a _dowildcard variable is defined as being
  -- * 0, therefore command line globbing is DISABLED by default. To turn it
  -- * on and to leave wildcard command line processing MS's globbing code,
  -- * include a line in one of your source modules defining _dowildcard and
  -- * setting it to -1, like so:
  -- * int _dowildcard = -1;
  --  

  -- Macros for __uuidof template-based emulation  
   --  skipped func __debugbreak

  -- mingw-w64 specific functions:  
   --  skipped func __mingw_get_crt_info

end umingw_h;
