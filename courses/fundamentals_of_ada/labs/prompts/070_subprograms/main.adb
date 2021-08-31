with Ada.Text_IO; use Ada.Text_IO;
procedure Main is

   type List_T is array (Integer range <>) of Integer;

   -- Is_Found will search a list for an item and will
   -- return True if the item is found in the list
   -- function Is_Found ( ?

   -- Sort will sort a list
   -- procedure Sort ( ?

   -- Fill will ask the user for values to place in the list
   -- procedure Fill ( ?

   Number : Integer;

begin

   Put ("Enter number of elements in list: ");
   Number := Integer'Value (Get_Line);

   declare
      List : List_T (1 .. Number);
   begin
      -- Fill the list with data
      -- Sort the list
      loop
         Put ("Enter number to look for: ");
         Number := Integer'Value (Get_Line);
         exit when Number < 0;
         -- Print True/False based on whether Number is in List
      end loop;
   end;

end Main;
