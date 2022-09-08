with Test_Suite;

-- Sprites
with Pixels.Tests;
with Surfaces.Tests;
with BMP_File_IO.Tests;

procedure Run_Test_Sprites is
begin
   
   Test_Suite.Run_All;
   
end Run_Test_Sprites;
