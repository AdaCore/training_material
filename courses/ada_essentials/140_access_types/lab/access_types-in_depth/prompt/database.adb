package body Database is

   function Create (Number : Positive; Symbol : Character) return Database_T is
      Retval : Database_T;
   begin
      return Retval;
   end Create;

   function Image (Value : Database_T) return String is
   begin
      return "";
   end Image;

   function "<" (L, R : Database_T) return Boolean is
   begin
      return False;
   end "<";

end Database;
