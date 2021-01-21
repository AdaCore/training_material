package Vstring is

   Max_String_Length : constant := 1_000;

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

   subtype Index_T is Integer range 0 .. Max_String_Length;
   type Vstring_T (Length : Index_T := 0) is record
      Text : String (1 .. Length);
   end record;

end Vstring;
