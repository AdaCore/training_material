--Strings
package Vstring is
   type Vstring_T is new string(1..1); -- not really!
   function To_Vstring
     (Str : String)
      return Vstring_T;
   function To_String
     (Vstr : Vstring_T)
      return String;
   function "&"
     (L, R : Vstring_T)
      return Vstring_T;
   function "&"
     (L : String;
      R : Vstring_T)
      return Vstring_T;
   function "&"
     (L : Vstring_T;
      R : String)
      return Vstring_T;
end Vstring;
