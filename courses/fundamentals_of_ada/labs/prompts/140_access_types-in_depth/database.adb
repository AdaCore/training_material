package body Database is
   function "="
     (L, R : Database_T)
      return Boolean is
   begin
      return False;
   end "=";

   function To_Database
     (Value : String)
      return Database_T is
      Retval : Database_T;
   begin
      return Retval;
   end To_Database;

   function From_Database
     (Value : Database_T)
      return String is
   begin
      return "";
   end From_Database;

   function "<"
     (L, R : Database_T)
      return Boolean is
   begin
      return False;
   end "<";
end Database;
