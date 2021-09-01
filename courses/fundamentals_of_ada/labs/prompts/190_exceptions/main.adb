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

   procedure Test_Square_Root (Number : Simple_Math.Float_T) is
   begin
      null;
   end Test_Square_Root;

   procedure Test_Square (Number : Simple_Math.Float_T) is
   begin
      null;
   end Test_Square;

   procedure Test_Multiply (Left  : Simple_Math.Float_T;
                            Right : Simple_Math.Float_T) is
   begin
      null;
   end Test_Multiply;

   procedure Test_Divide (Numerator   : Simple_Math.Float_T;
                          Denominator : Simple_Math.Float_T) is
   begin
      null;
   end Test_Divide;

begin

   Put_Line ("Test_Square_Root");
   Test_Square_Root (Get ("Number"));
   Put_Line ("Test_Square");
   Test_Square (Get ("Number"));
   Put_Line ("Test_Multiply");
   Test_Multiply (Get ("Left"), Get ("right"));
   Put_Line ("Test_Divide");
   Test_Divide (Get ("Numerator"), Get ("Denominator"));

end Main;
