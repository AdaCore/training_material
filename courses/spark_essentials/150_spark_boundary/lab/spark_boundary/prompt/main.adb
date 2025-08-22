with Ada.Text_IO;

procedure Main is

   procedure Swap (X, Y : in out Integer)
     with Import, Convention => C;

   X : Integer := 1;
   Y : Integer := 2;
begin
   Ada.Text_IO.Put_Line ("X =" & X'Img & "; Y =" & Y'Img);
   Swap (X, Y);
   Ada.Text_IO.Put_Line ("X =" & X'Img & "; Y =" & Y'Img);
end Main;
