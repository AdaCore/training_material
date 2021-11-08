with Keys_Pkg;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is

   Keys : array (1 .. 3) of Keys_Pkg.Key_T;

   procedure Generate (Count : Natural) is
      Keys : array (1 .. Count) of Keys_Pkg.Key_T;
   begin
      Put_Line ("In use: " & Integer'Image (Keys_Pkg.In_Use));
      for Key of Keys
      loop
         Put_Line ("   " & Keys_Pkg.Image (Key));
      end loop;
   end Generate;

begin

   Put_Line ("In use: " & Integer'Image (Keys_Pkg.In_Use));

   Generate (4);

   Put_Line ("In use: " & Integer'Image (Keys_Pkg.In_Use));

end Main;
