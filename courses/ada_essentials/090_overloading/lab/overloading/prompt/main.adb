with Ada.Text_IO; use Ada.Text_IO;
procedure Main is

   --  Implement these types correctl
   type Digit_T is new Integer;
   type Digit_Name_T is new Integer;

   --  One conversion function (not implemented!)
   --  Create as many as you need
   function Convert (Value : Digit_T) return Digit_Name_T is
   begin
      return Digit_Name_T'First;
   end Convert;

   --  Create operator functions to add objects of Digit_T
   --  and Digit_Name_T and return an integer

begin

   null;

   --  Print the result of resolving these additions

   --  One + 2
   --  3 + Four
   --  Five + Six
   --  7 + 8
end Main;
