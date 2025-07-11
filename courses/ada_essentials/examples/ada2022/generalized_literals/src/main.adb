with Ada.Text_IO;
with My_Int; use My_Int;

procedure Main is
   --$ begin cut
   I : My_Int_T := 1;
   --$ end cut
begin
   Ada.Text_IO.Put_Line (Image (I));
end Main;
