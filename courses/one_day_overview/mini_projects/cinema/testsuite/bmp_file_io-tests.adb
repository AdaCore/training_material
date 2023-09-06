with Ada.Command_Line;
with Ada.Directories;
use Ada;

package body BMP_File_IO.Tests is
   
   Root : constant String
     := Directories.Containing_Directory (Directories.Containing_Directory (Command_Line.Command_Name));

   procedure Test_Get_Square_10x10 (TC : Test_Case_T'Class) is
      F : File_Type;
   begin
      Open (F, In_File, Root & "/resources/dummy_square_10x10.bmp");
      
      declare
         Surf : Surfaces.Surface_T := Get (F);
      begin
         pragma Assert (Surf'Length (1) = 10);
         pragma Assert (Surf'Length (2) = 10);
      exception
         when others =>
            Close (F);
            raise;
      end;
      
      Close (F);
   end Test_Get_Square_10x10;

end BMP_File_IO.Tests;
