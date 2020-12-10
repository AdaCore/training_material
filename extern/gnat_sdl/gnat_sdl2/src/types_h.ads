pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with umingw_h;
with Interfaces.C.Extensions;

package types_h is

  --*
  -- * This file has no copyright assigned and is placed in the Public Domain.
  -- * This file is part of the mingw-w64 runtime package.
  -- * No warranty is given; refer to the file DISCLAIMER.PD within this package.
  --  

   subtype u_ino_t is unsigned_short;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\sys\types.h:43

   subtype ino_t is unsigned_short;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\sys\types.h:45

   subtype u_dev_t is unsigned;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\sys\types.h:51

   subtype dev_t is unsigned;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\sys\types.h:53

   subtype u_pid_t is Long_Long_Integer;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\sys\types.h:63

   subtype pid_t is u_pid_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\sys\types.h:68

   subtype u_mode_t is unsigned_short;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\sys\types.h:74

   subtype mode_t is u_mode_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\sys\types.h:77

   subtype useconds_t is unsigned;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\sys\types.h:84

  -- Seconds  
   type timespec is record
      tv_sec : aliased umingw_h.time_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\sys\types.h:90
      tv_nsec : aliased long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\sys\types.h:91
   end record;
   pragma Convention (C_Pass_By_Copy, timespec);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\sys\types.h:89

  -- Nanoseconds  
  -- Timer period  
   type itimerspec is record
      it_interval : aliased timespec;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\sys\types.h:95
      it_value : aliased timespec;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\sys\types.h:96
   end record;
   pragma Convention (C_Pass_By_Copy, itimerspec);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\sys\types.h:94

  -- Timer expiration  
   subtype u_sigset_t is Extensions.unsigned_long_long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\sys\types.h:104

   subtype sigset_t is u_sigset_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\sys\types.h:109

end types_h;
