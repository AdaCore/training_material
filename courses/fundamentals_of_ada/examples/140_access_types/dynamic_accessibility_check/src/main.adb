procedure Main is
   type Acc is access all Integer;
   O : Acc;

   procedure Set_Value (V : access Integer) is
   begin
      O := Acc (V);
   end Set_Value;
begin
   declare
      O2 : aliased Integer := 2;
   begin
      Set_Value (O2'Access);
   end;
end Main;
