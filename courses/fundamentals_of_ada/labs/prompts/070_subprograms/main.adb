with Ada.Text_IO; use Ada.Text_IO;
procedure Main is

   type List_T is array (Positive range <>) of Integer;

   --  Search should return the index where the item
   --  should be placed in the list
   function Search return Positive is
   begin
      return 1;
   end Search;

   --  Add will add an item to the appropriate spot in the list
   procedure Add is
   begin
      null;
   end Add;

begin

   --  Add some items to the list
   --  (make sure they're not already in order!)

   --  Sort the list (if not already sorted)

   --  Print the list

   null;

end Main;
