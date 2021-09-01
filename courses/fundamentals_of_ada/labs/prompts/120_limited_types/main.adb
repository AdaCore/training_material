
with Ada.Text_IO; use Ada.Text_IO;
with Timecards;
procedure Main is

   -- populate these somehow
   One : Timecards.Timecard_T;
   Two : Timecards.Timecard_T;

begin
   Put_Line ( Timecards.Image ( One ) );
   Put_Line ( Timecards.Image ( Two ) );
end Main;
--Main
