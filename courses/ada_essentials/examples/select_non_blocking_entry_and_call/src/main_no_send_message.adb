with Ada.Text_IO; use Ada.Text_IO;
with Tasks;       use Tasks;

procedure Main_No_Send_Message is
begin
   --$ begin cut
   T.Start;
   T.Stop;
   --$ end cut
end Main_No_Send_Message;
