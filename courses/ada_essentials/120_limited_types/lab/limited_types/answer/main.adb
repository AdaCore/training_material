with Ada.Text_IO; use Ada.Text_IO;
with Timecards;
procedure Main is

   One : constant Timecards.Timecard_T := Timecards.Create
       (Name  => "Fred  ",
        Rate  => 1.1,
        Hours => 2.2);
   Two : constant Timecards.Timecard_T := Timecards.Create
       (Name  => "Barney",
        Rate  => 3.3,
        Hours => 4.4);

begin
   Put_Line (Timecards.Image (One));
   Put_Line (Timecards.Image (Two));
end Main;
