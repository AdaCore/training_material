with Ada.Text_IO; use Ada.Text_IO;
with Task_Select; use Task_Select;

procedure Main is
begin
   Put_Line ("calling start");
   Select_Loop_Task.Start;
   Select_Loop_Task.Receive_Message ("1");
   Select_Loop_Task.Send_Message ("A");
   Select_Loop_Task.Send_Message ("B");
   Select_Loop_Task.Receive_Message ("2");
   Select_Loop_Task.Stop;
exception
   when Tasking_Error =>
      Put_Line ("Expected exception: Entry not reached");
end Main;
