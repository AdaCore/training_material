pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package umingw_off_t_h is

   subtype u_off_t is long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\_mingw_off_t.h:5

   subtype off32_t is long;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\_mingw_off_t.h:7

   subtype u_off64_t is Long_Long_Integer;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\_mingw_off_t.h:13

   subtype off64_t is Long_Long_Integer;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\_mingw_off_t.h:15

   subtype off_t is off32_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\_mingw_off_t.h:26

end umingw_off_t_h;
