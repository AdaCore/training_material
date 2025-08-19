--Database
with Ada.Strings.Unbounded;
package Database is
   type Database_T is private;
   function "="
     (L, R : Database_T)
      return Boolean;
   function To_Database
     (Value : String)
      return Database_T;
   function From_Database
     (Value : Database_T)
      return String;
   function "<"
     (L, R : Database_T)
      return Boolean;
private
   type Database_T is null record;
end Database;
