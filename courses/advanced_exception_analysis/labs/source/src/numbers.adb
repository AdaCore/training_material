with Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
package body Numbers is

   function Convert
     (Number : Quantity_T)
      return String is
      Retval : constant String := Number'image;
   begin
      return Retval (2 .. Retval'last);
   end Convert;

   function Convert
     (Number : String)
      return Quantity_T is
   begin
      return Quantity_T'value (Number);
   end Convert;

   package IO is new Ada.Text_IO.Float_IO (Cost_T);
   function Convert
     (Number : Cost_T)
      return String is
      Retval : String (1 .. 10);
   begin
      IO.Put (Retval, Number, 2, 0);
      return Trim (Retval, Ada.Strings.Both);
   end Convert;

   function Convert
     (Number : String)
      return Cost_T is
   begin
      return Cost_T'value (Number);
   end Convert;

end Numbers;
