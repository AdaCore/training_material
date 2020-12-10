pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;

package vadefs_h is

   subtype uu_gnuc_va_list is System.Address;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/vadefs.h:24

   subtype va_list is uu_gnuc_va_list;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/vadefs.h:31

end vadefs_h;
