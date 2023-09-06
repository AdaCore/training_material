with Test_Suite;

pragma Warnings (Off, "not referenced");
-- Display
with Drawable_Chars.Tests;
with Char_Display_Driver.Tests;
pragma Warnings (On, "not referenced");

procedure Run_Test_Display is
begin

   Test_Suite.Run_All;

end Run_Test_Display;
