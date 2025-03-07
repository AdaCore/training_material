with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with Math;             use Math;

procedure Main is
   One    : Integer := Integer'Value (Ada.Command_Line.Argument (1));
   Two    : Integer := Integer'Value (Ada.Command_Line.Argument (2));
   Three  : Integer := Integer'Value (Ada.Command_Line.Argument (3));
   Result : Integer;
begin
   Add_Two_Numbers (Result, One, Two);
   Result := Subtract_Two_Numbers (Result, Three);
   Put_Line
     (One'Image & " + " & Two'Image & " - " & Three'Image & " = " &
      Result'Image);
end Main;
