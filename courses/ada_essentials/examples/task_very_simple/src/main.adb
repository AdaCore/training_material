with Ada.Text_IO; use Ada.Text_IO;
with Tasks;       use Tasks;

procedure Main is
begin
   Put_Line ("calling start");
   T.Start;
   Put_Line ("calling receive 1");
   T.Receive_Message ("1");
   Put_Line ("calling receive 2");
   --  Locks until somebody calls Start
   T.Receive_Message ("2");
end Main;
