
package body Database is
   function "=" (L, R : Database_T) return Boolean is
   begin
      return L.Value (1 .. L.Length) = R.Value (1 .. R.Length);
   end "=";
   function To_Database (Value : String) return Database_T is
      Retval : Database_T;
   begin
      Retval.Length                     := Value'Length;
      Retval.Value (1 .. Retval.Length) := Value;
      return Retval;
   end To_Database;
   function From_Database (Value : Database_T) return String is
   begin
      return Value.Value (1 .. Value.Length);
   end From_Database;

   function "<" (L, R : Database_T) return Boolean is
   begin
      return L.Value (1 .. L.Length) < R.Value (1 .. R.Length);
   end "<";
end Database;
