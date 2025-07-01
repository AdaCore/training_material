package body Vstring is
   function To_Vstring
     (Str : String)
      return Vstring_T is
   begin
      return (others => ' ');
   end To_Vstring;
   function To_String
     (Vstr : Vstring_T)
      return String is
   begin
      return "";
   end To_String;
   function "&"
     (L, R : Vstring_T)
      return Vstring_T is
   begin
      return (others => ' ');
   end "&";
   function "&"
     (L : String;
      R : Vstring_T)
      return Vstring_T is
   begin
      return (others => ' ');
   end "&";
   function "&"
     (L : Vstring_T;
      R : String)
      return Vstring_T is
   begin
      return (others => ' ');
   end "&";
end Vstring;
