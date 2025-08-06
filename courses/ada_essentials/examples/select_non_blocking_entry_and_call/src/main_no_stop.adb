with Ada.Text_IO; use Ada.Text_IO;
with Tasks;       use Tasks;

procedure Main_No_Stop is
begin
   --$ begin cut
   select
      T.Stop;
   else
      Put_Line ("No stop");
   end select;
   --$ end cut

   T.Start;
   T.Stop;
end Main_No_Stop;
