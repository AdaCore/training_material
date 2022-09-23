with Ada.Text_IO;
with My_Int; use My_Int;

procedure Main is
    I : My_Int_T := 1;
begin
    Ada.Text_IO.Put_Line (Image (I));
end Main;
