pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;

package guiddef_h is

   --  unsupported macro: IID_NULL GUID_NULL
   --  unsupported macro: CLSID_NULL GUID_NULL
   --  unsupported macro: FMTID_NULL GUID_NULL
   --  arg-macro: procedure IsEqualFMTID (rfmtid1, rfmtidIsEqualGUID(rfmtid1,rfmtid2)
   --    IsEqualGUID(rfmtid1,rfmtid2)
   --  unsupported macro: REFGUID const GUID &
   --  unsupported macro: REFIID const IID &
   --  unsupported macro: REFCLSID const IID &
   --  unsupported macro: REFFMTID const IID &
   --  arg-macro: procedure IsEqualIID (riid1, riid2)
   --    IsEqualGUID(riid1,riid2)
   --  arg-macro: procedure IsEqualCLSID (rclsid1, rclsidIsEqualGUID(rclsid1,rclsid2)
   --    IsEqualGUID(rclsid1,rclsid2)
   --  arg-macro: procedure DEFINE_GUID (name, l, w1, w2EXTERN_C const GUID name
   --    EXTERN_C const GUID name
   --  arg-macro: procedure DEFINE_OLEGUID (name, l, w1, w2DEFINE_GUID(name,l,w1,w2,16#C0#,0,0,0,0,0,0,16#46#)
   --    DEFINE_GUID(name,l,w1,w2,16#C0#,0,0,0,0,0,0,16#46#)
   type u_GUID_Data4_array is array (0 .. 7) of aliased unsigned_char;
   type u_GUID is record
      Data1 : aliased unsigned_long;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/guiddef.h:9
      Data2 : aliased unsigned_short;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/guiddef.h:10
      Data3 : aliased unsigned_short;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/guiddef.h:11
      Data4 : aliased u_GUID_Data4_array;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/guiddef.h:12
   end record;
   pragma Convention (C_Pass_By_Copy, u_GUID);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/guiddef.h:8

   subtype GUID is u_GUID;

   type LPGUID is access all GUID;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/guiddef.h:59

   type LPCGUID is access constant GUID;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/guiddef.h:64

   subtype IID is u_GUID;

   type LPIID is access all IID;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/guiddef.h:71

   subtype CLSID is u_GUID;

   type LPCLSID is access all CLSID;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/guiddef.h:80

   subtype FMTID is u_GUID;

   type LPFMTID is access all FMTID;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/guiddef.h:85

   function InlineIsEqualGUID (rguid1 : System.Address; rguid2 : System.Address) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/guiddef.h:138
   pragma Import (C, InlineIsEqualGUID, "InlineIsEqualGUID");

   function IsEqualGUID (rguid1 : System.Address; rguid2 : System.Address) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/guiddef.h:142
   pragma Import (C, IsEqualGUID, "IsEqualGUID");

   function operator_eq (guidOne : System.Address; guidOther : System.Address) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/guiddef.h:159
   pragma Import (CPP, operator_eq, "_Zeq");

   function operator_ne (guidOne : System.Address; guidOther : System.Address) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/guiddef.h:160
   pragma Import (CPP, operator_ne, "_Zne");

end guiddef_h;
