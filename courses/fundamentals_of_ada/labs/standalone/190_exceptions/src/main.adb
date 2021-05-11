with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Simple_Math;
procedure Main is

  package Io is new Ada.Text_IO.Float_IO (Simple_Math.Float_T);

  function Get
   (Prompt : String)
    return Simple_Math.Float_T is
  begin
    Put ("  " & Prompt & " => ");
    return Simple_Math.Float_T'Value (Get_Line);
  end Get;

begin

  Put_Line ("Square Root");
  declare
    Number : Simple_Math.Float_T := Get ("Number");
  begin
    Io.Put
     (Simple_Math.Sqrt (Number),
      Exp => 0);
    New_Line;
  exception
    when Error : Simple_Math.Illegal_Operation =>
      Put_Line (Ada.Exceptions.Exception_Message (Error));
    when Error : others =>
      Put_Line (Ada.Exceptions.Exception_Information (Error));
  end;

  Put_Line ("Square");
  declare
    Number : Simple_Math.Float_T := Get ("Number");
  begin
    Io.Put
     (Simple_Math.Square (Number),
      Exp => 0);
    New_Line;
  exception
    when Error : Simple_Math.Illegal_Operation =>
      Put_Line (Ada.Exceptions.Exception_Message (Error));
    when Error : others =>
      Put_Line (Ada.Exceptions.Exception_Information (Error));
  end;

  Put_Line ("Multiply");
  declare
    Left  : Simple_Math.Float_T := Get ("Left");
    Right : Simple_Math.Float_T := Get ("Right");
  begin
    Io.Put
     (Simple_Math.Multiply (Left, Right),
      Exp => 0);
    New_Line;
  exception
    when Error : Simple_Math.Illegal_Operation =>
      Put_Line (Ada.Exceptions.Exception_Message (Error));
    when Error : others =>
      Put_Line (Ada.Exceptions.Exception_Information (Error));
  end;

  Put_Line ("Divide");
  declare
    Numerator   : Simple_Math.Float_T := Get ("Numerator");
    Denominator : Simple_Math.Float_T := Get ("Denominator");
  begin
    Io.Put
     (Simple_Math.Divide (Numerator, Denominator),
      Exp => 0);
    New_Line;
  exception
    when Error : Simple_Math.Illegal_Operation =>
      Put_Line (Ada.Exceptions.Exception_Message (Error));
    when Error : others =>
      Put_Line (Ada.Exceptions.Exception_Information (Error));
  end;

exception
  when The_Err : others =>
    Put_Line
     ("Other error: " & Ada.Exceptions.Exception_Information (The_Err));

end Main;
