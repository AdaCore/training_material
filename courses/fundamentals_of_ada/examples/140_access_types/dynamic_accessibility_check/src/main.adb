with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
--start_snippet
   type Acc is access all Integer;
   O : Acc;
   Outer : aliased Integer := 123;
   procedure Set_Value (V : access Integer) is
   begin
      Put_Line (V.all'Image);
      O := Acc (V);
   end Set_Value;
begin
   Set_Value (Outer'Access);
   declare
      Inner : aliased Integer := 987;
   begin
      Set_Value (Inner'Access);
   end;
--end_snippet
end Main;
