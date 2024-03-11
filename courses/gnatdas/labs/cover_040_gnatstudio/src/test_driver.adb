with Ada.Text_IO; use Ada.Text_IO;
with Ops;
procedure Test_Driver is
   procedure Run_One
     (Kind  : Ops.Op_Kind;
      Value : Integer) is
      X : Integer := Value;
   begin
      Ops.Apply (Kind, X);
      Put_Line ("Before:" & Value'Image & " After:" & X'Image);
   end Run_One;
begin
   Run_One (Ops.Increment, 4);
end Test_Driver;
