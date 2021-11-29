--Strings
package Vstring is
   type Vstring_T is private;
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
private
   -- implement this as a variant record
   type Vstring_T is new String (1 .. 1_000);
end Vstring;
