with Test_Suite;

pragma Warnings (Off, "not referenced");
-- Sprites
with Pixels.Tests;
with Surfaces.Tests;
with BMP_File_IO.Tests;

-- Display
with Drawable_Chars.Tests;
with Char_Display_Driver.Tests;
pragma Warnings (On, "not referenced");

procedure Run_Tests is
begin
   Test_Suite.Run_All;
end Run_Tests;
