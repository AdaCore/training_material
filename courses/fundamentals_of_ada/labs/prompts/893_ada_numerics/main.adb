with Ada.Text_IO; use Ada.Text_IO;
procedure Main is

   package Io is new Float_IO (Float);

   type Index_T is range 1 .. 10;
   type Element_T is null record; -- fix this
   Values : array (Index_T) of Element_T;

begin
   for I in 1 .. 100 loop -- bigger than 100 is good
      null;
      -- Find a random array index
      -- Set the indexed element as specified
   end loop;

   for I in Index_T'Range loop
      -- Print index, number of values added, and average of total
      New_Line;
   end loop;
end Main;
