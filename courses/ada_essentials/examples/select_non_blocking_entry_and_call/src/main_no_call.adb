with Ada.Text_IO; use Ada.Text_IO;
with Tasks;       use Tasks;

procedure Main_No_Call is
begin
   T.Start;

   --$ begin cut
   select
      T.Receive_Message ("1");
   else
      Put_Line ("No message sent");
   end select;
   --$ end cut

   T.Stop;
end Main_No_Call;
