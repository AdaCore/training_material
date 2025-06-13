with Ada.Text_IO;

procedure Main is

   procedure Swap (X, Y : in out Integer)
   with
     Import,
     Convention => C,
     Global => null,
     Always_Terminates,
     Post => X = Y'Old and then Y = X'Old;

   X : Integer := 1;
   Y : Integer := 2;
begin
   Ada.Text_IO.Put_Line ("X =" & X'Img & "; Y =" & Y'Img);
   Swap (X, Y);
   pragma Assert (X = 2 and Y = 1);
   Ada.Text_IO.Put_Line ("X =" & X'Img & "; Y =" & Y'Img);
end Main;
