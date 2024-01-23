with Ada.Text_IO; use Ada.Text_IO;
with Examples;
procedure Main is
   type Test_Procedure_T is access procedure
       (A, B, C :     Integer;
        Z       : out Integer);
   procedure Test
     (Name    : String;
      A, B, C : Integer;
      Proc    : Test_Procedure_T) is
      Z : Integer;
   begin
      Put_Line (Name & " => " & A'Image & ", " & B'Image & ", " & C'Image);
      begin
         Proc (A, B, C, Z);
         begin
            Put_Line ("   result: " & Z'Image);
         exception
            when others =>
               Put_Line ("   result invalid");
         end;
      exception
         when others =>
            Put_Line ("   call failed");
      end;
   end Test;

begin
   Test ("Test_Statement", 1, 2, Integer'Last, Examples.Test_Statement'Access);
   Test
     ("Test_Decision True", 1, 1, Integer'Last, Examples.Test_Decision'Access);
   Test ("Test_Decision False", 0, 0, 0, Examples.Test_Decision'Access);
   Test ("Test_Mcdc A > 0, B = 0", 1, 0, 0, Examples.Test_Mcdc'Access);
   Test ("Test_Mcdc A = 0, B > 0", 1, 0, 0, Examples.Test_Mcdc'Access);
   Test ("Test_Mcdc A > 0, B > 0", 1, 1, 1, Examples.Test_Mcdc'Access);
   Test ("Test_Mcdc A, B, C = -1", -1, -1, -1, Examples.Test_Mcdc'Access);

end Main;
