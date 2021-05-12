with Ada.Command_Line;
use Ada.Command_Line;
with Ada.Text_IO;
use Ada.Text_IO;
with Fibonacci;
procedure Main is
   Limit : Natural := 0;
begin
   if Argument_Count < 1 then
      Put_Line ("Usage: " & Command_Name & " <positive count>");
   else
      begin
         Limit := Positive'Value (Argument (1));
      exception
         when others =>
            Put_Line ("<positive count> was not legal");
      end;
      if Limit > 0 then
         Fibonacci.Perform (Limit);
      end if;
   end if;
end Main;
