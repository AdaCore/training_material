pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with umingw_h;

package ctype_h is

   --  unsupported macro: isascii __isascii
   --  unsupported macro: toascii __toascii
   --  unsupported macro: iscsymf __iscsymf
   --  unsupported macro: iscsym __iscsym
   --  skipped func _isctype

   --  skipped func _isctype_l

   function isalpha (u_C : int) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:102
   pragma Import (C, isalpha, "isalpha");

   --  skipped func _isalpha_l

   function isupper (u_C : int) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:104
   pragma Import (C, isupper, "isupper");

   --  skipped func _isupper_l

   function islower (u_C : int) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:106
   pragma Import (C, islower, "islower");

   --  skipped func _islower_l

   function isdigit (u_C : int) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:108
   pragma Import (C, isdigit, "isdigit");

   --  skipped func _isdigit_l

   function isxdigit (u_C : int) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:110
   pragma Import (C, isxdigit, "isxdigit");

   --  skipped func _isxdigit_l

   function isspace (u_C : int) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:112
   pragma Import (C, isspace, "isspace");

   --  skipped func _isspace_l

   function ispunct (u_C : int) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:114
   pragma Import (C, ispunct, "ispunct");

   --  skipped func _ispunct_l

   function isalnum (u_C : int) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:116
   pragma Import (C, isalnum, "isalnum");

   --  skipped func _isalnum_l

   function isprint (u_C : int) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:118
   pragma Import (C, isprint, "isprint");

   --  skipped func _isprint_l

   function isgraph (u_C : int) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:120
   pragma Import (C, isgraph, "isgraph");

   --  skipped func _isgraph_l

   function iscntrl (u_C : int) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:122
   pragma Import (C, iscntrl, "iscntrl");

   --  skipped func _iscntrl_l

   function toupper (u_C : int) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:124
   pragma Import (C, toupper, "toupper");

   function tolower (u_C : int) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:125
   pragma Import (C, tolower, "tolower");

   --  skipped func _tolower

   --  skipped func _tolower_l

   --  skipped func _toupper

   --  skipped func _toupper_l

   function isblank (u_C : int) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:136
   pragma Import (C, isblank, "isblank");

   function iswalpha (u_C : umingw_h.wint_t) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:143
   pragma Import (C, iswalpha, "iswalpha");

   --  skipped func _iswalpha_l

   function iswupper (u_C : umingw_h.wint_t) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:145
   pragma Import (C, iswupper, "iswupper");

   --  skipped func _iswupper_l

   function iswlower (u_C : umingw_h.wint_t) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:147
   pragma Import (C, iswlower, "iswlower");

   --  skipped func _iswlower_l

   function iswdigit (u_C : umingw_h.wint_t) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:149
   pragma Import (C, iswdigit, "iswdigit");

   --  skipped func _iswdigit_l

   function iswxdigit (u_C : umingw_h.wint_t) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:151
   pragma Import (C, iswxdigit, "iswxdigit");

   --  skipped func _iswxdigit_l

   function iswspace (u_C : umingw_h.wint_t) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:153
   pragma Import (C, iswspace, "iswspace");

   --  skipped func _iswspace_l

   function iswpunct (u_C : umingw_h.wint_t) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:155
   pragma Import (C, iswpunct, "iswpunct");

   --  skipped func _iswpunct_l

   function iswalnum (u_C : umingw_h.wint_t) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:157
   pragma Import (C, iswalnum, "iswalnum");

   --  skipped func _iswalnum_l

   function iswprint (u_C : umingw_h.wint_t) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:159
   pragma Import (C, iswprint, "iswprint");

   --  skipped func _iswprint_l

   function iswgraph (u_C : umingw_h.wint_t) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:161
   pragma Import (C, iswgraph, "iswgraph");

   --  skipped func _iswgraph_l

   function iswcntrl (u_C : umingw_h.wint_t) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:163
   pragma Import (C, iswcntrl, "iswcntrl");

   --  skipped func _iswcntrl_l

   function iswascii (u_C : umingw_h.wint_t) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:165
   pragma Import (C, iswascii, "iswascii");

   function isleadbyte (u_C : int) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:166
   pragma Import (C, isleadbyte, "isleadbyte");

   --  skipped func _isleadbyte_l

   function towupper (u_C : umingw_h.wint_t) return umingw_h.wint_t;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:168
   pragma Import (C, towupper, "towupper");

   --  skipped func _towupper_l

   function towlower (u_C : umingw_h.wint_t) return umingw_h.wint_t;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:170
   pragma Import (C, towlower, "towlower");

   --  skipped func _towlower_l

   function iswctype (u_C : umingw_h.wint_t; u_Type : umingw_h.wctype_t) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:172
   pragma Import (C, iswctype, "iswctype");

   --  skipped func _iswctype_l

   --  skipped func _iswcsymf_l

   --  skipped func _iswcsym_l

   function is_wctype (u_C : umingw_h.wint_t; u_Type : umingw_h.wctype_t) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:178
   pragma Import (C, is_wctype, "is_wctype");

   function iswblank (u_C : umingw_h.wint_t) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/ctype.h:181
   pragma Import (C, iswblank, "iswblank");

end ctype_h;
