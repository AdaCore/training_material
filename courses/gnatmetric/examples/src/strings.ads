package Strings is
   type String_T is tagged private;
   function "&" (L, R : String_T) return String_T;
   function To_String (S : String_T) return String;
   function From_String (S : String) return String_T;
private
   Maximum_Length : constant := 100;
   subtype Index_T is Natural range 0 .. Maximum_Length;
   type String_T is tagged record
      Length : Index_T := 0;
      Text   : String (1 .. Maximum_Length);
   end record;
end Strings;
