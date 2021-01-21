
package body Vstring is

   function To_Vstring
     (Str : String)
      return Vstring_T is ((Length => Str'Length, Text => Str));
   function To_String
     (Vstr : Vstring_T)
      return String is (Vstr.Text);
   function "&"
     (L, R : Vstring_T)
      return Vstring_T is
      Ret_Val : constant String := L.Text & R.Text;
   begin
      return (Length => Ret_Val'Length, Text => Ret_Val);
   end "&";

   function "&"
     (L : String;
      R : Vstring_T)
      return Vstring_T is
      Ret_Val : constant String := L & R.Text;
   begin
      return (Length => Ret_Val'Length, Text => Ret_Val);
   end "&";

   function "&"
     (L : Vstring_T;
      R : String)
      return Vstring_T is
      Ret_Val : constant String := L.Text & R;
   begin
      return (Length => Ret_Val'Length, Text => Ret_Val);
   end "&";

end Vstring;
