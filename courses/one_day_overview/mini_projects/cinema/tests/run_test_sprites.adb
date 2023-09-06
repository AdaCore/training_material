with Test_Suite;

pragma Warnings (Off, "not referenced");
-- Sprites
with Pixels.Tests;
with Surfaces.Tests;
with BMP_File_IO.Tests;
pragma Warnings (On, "not referenced");

procedure Run_Test_Sprites is
begin
   
   Test_Suite.Run_All;
   
end Run_Test_Sprites;
