pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with stddef_h;
with Interfaces.C.Strings;

package crtdefs_h is

  --*
  -- * This file has no copyright assigned and is placed in the Public Domain.
  -- * This file is part of the mingw-w64 runtime package.
  -- * No warranty is given; refer to the file DISCLAIMER.PD within this package.
  --  

  -- #define __ERRCODE_DEFINED_MS  
   subtype rsize_t is stddef_h.size_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:26

   type threadmbcinfostruct is null record;   -- incomplete struct

   type threadlocaleinfostruct;
   type pthreadlocinfo is access all threadlocaleinfostruct;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:155

   type pthreadmbcinfo is access all threadmbcinfostruct;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:156

   type uu_lc_time_data is null record;   -- incomplete struct

   type localeinfo_struct is record
      locinfo : pthreadlocinfo;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:160
      mbcinfo : pthreadmbcinfo;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:161
   end record;
   pragma Convention (C_Pass_By_Copy, localeinfo_struct);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:159

   subtype u_locale_tstruct is localeinfo_struct;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:162

   type u_locale_t is access all localeinfo_struct;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:162

   type tagLC_ID is record
      wLanguage : aliased unsigned_short;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:167
      wCountry : aliased unsigned_short;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:168
      wCodePage : aliased unsigned_short;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:169
   end record;
   pragma Convention (C_Pass_By_Copy, tagLC_ID);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:166

   subtype LC_ID is tagLC_ID;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:170

   type LPLC_ID is access all tagLC_ID;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:170

   type anon_1 is record
      locale : Interfaces.C.Strings.chars_ptr;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:182
      wlocale : access wchar_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:183
      refcount : access int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:184
      wrefcount : access int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:185
   end record;
   pragma Convention (C_Pass_By_Copy, anon_1);
   type threadlocaleinfostruct_lc_handle_array is array (0 .. 5) of aliased unsigned_long;
   type threadlocaleinfostruct_lc_id_array is array (0 .. 5) of aliased LC_ID;
   type threadlocaleinfostruct_lc_category_array is array (0 .. 5) of aliased anon_1;
   type lconv;
   type threadlocaleinfostruct is record
      refcount : aliased int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:176
      lc_codepage : aliased unsigned;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:177
      lc_collate_cp : aliased unsigned;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:178
      lc_handle : aliased threadlocaleinfostruct_lc_handle_array;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:179
      lc_id : aliased threadlocaleinfostruct_lc_id_array;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:180
      lc_category : aliased threadlocaleinfostruct_lc_category_array;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:186
      lc_clike : aliased int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:187
      mb_cur_max : aliased int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:188
      lconv_intl_refcount : access int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:189
      lconv_num_refcount : access int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:190
      lconv_mon_refcount : access int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:191
      the_lconv : access lconv;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:192
      ctype1_refcount : access int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:193
      ctype1 : access unsigned_short;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:194
      pctype : access unsigned_short;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:195
      pclmap : access unsigned_char;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:196
      pcumap : access unsigned_char;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:197
      lc_time_curr : access uu_lc_time_data;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:198
   end record;
   pragma Convention (C_Pass_By_Copy, threadlocaleinfostruct);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:175

   type lconv is null record;   -- incomplete struct

   subtype threadlocinfo is threadlocaleinfostruct;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\crtdefs.h:199

end crtdefs_h;
