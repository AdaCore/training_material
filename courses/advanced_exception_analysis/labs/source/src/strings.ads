package Strings is

   type String_T is access all String;
   function Convert
     (Str : String_T)
      return String;
   function Convert
     (Str : String)
      return String_T;

   type Strings_T is array (Positive range <>) of String_T;
   function Split
     (Str       : String;
      Separator : String := "|")
      return Strings_T;

end Strings;
