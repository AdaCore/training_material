package body Data_Type is

   function "<" (L, R : Record_T) return Boolean is
   begin
      return False;
   end "<";

   function Image (Element : Record_T) return String is
   begin
      return "";
   end Image;

end Data_Type;
