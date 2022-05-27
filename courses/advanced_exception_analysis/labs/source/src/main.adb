with Ada.Command_Line; use Ada.Command_Line;
with Parser;           use Parser;
procedure Main is
begin
   Load (Ada.Command_Line.Argument (1));
   Print;
end Main;
