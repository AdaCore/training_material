package Syntax_Metrics_Example is

   Maximum_Length : constant := 1_000;
   subtype Length_T is Integer range 0 .. Maximum_Length;
   type String_T is record
      Length : Length_T := 0;
      Text   : String (1 .. Maximum_Length);
   end record;

   function "&"
     (L, R : String_T)
      return String_T;

   function To_String
     (S : String_T)
      return String;

   function From_String
     (S : String)
      return String_T;

end Syntax_Metrics_Example;
