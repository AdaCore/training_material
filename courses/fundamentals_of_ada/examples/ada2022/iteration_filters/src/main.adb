with Ada.Text_IO;

procedure Main is
begin
   for J in 0 .. 10 when J mod 2 /= 0 loop
      Ada.Text_IO.Put_Line (J'Img);
   end loop;
end Main;
