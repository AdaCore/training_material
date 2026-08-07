with Basics;      use Basics;
with Ada.Text_IO; use Ada.Text_IO;
procedure Test_Program is
   The_Array : constant Arr := (10, 1, 9, 2, 8, 3, 7, 4, 6, 5);

   procedure Run_One (Val : Element) is
      At_Index : Index;
      Status   : Boolean;
   begin
      Search (The_Array, Val, At_Index, Status);
      if Status then
         Put_Line ("Found" & Val'Image & " at" & At_Index'Image);
      else
         Put_Line ("Did not find" & Val'Image);
      end if;
   end Run_One;

begin

   Run_One (1);
   Run_One (3);
   Run_One (10);
   Run_One (-1);
   Run_One (11);

end Test_Program;
