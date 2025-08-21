--Database
package Database is
   type Database_T is private;
   function "=" (L, R : Database_T) return Boolean;
   function To_Database (Value : String) return Database_T;
   function From_Database (Value : Database_T) return String;
   function "<" (L, R : Database_T) return Boolean;
private
   type Database_T is record
      Value  : String (1 .. 100);
      Length : Natural;
   end record;
end Database;
