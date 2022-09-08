with Drawable_Chars.Latin_1;
with Pixels;

package body Char_Display_Driver.Tests is

   procedure Test_Get_Row (TC : Test_Case_T'Class) is
      type Palette_T is array (Positive range <>) of Pixels.Pixel_T;
      P : Palette_T :=
        ((0, 0, 0, 255), (16, 16, 16, 255), (32, 32, 32, 255), (48, 48, 48, 255), (100, 100, 100, 255));
      CD : Char_Display := 
        (Rows => 1,
         Columns => 9,
         Lux_Charset => Drawable_Chars.Latin_1.By_Black,
         Surf =>
           (1 => (P (1), P (2), P(3), P(4), P (5), P (4), P (3), P (2), P (1))));
   begin
      pragma Assert (Get_Row (CD, 1) = "  ..--~~xx~~--..  ", Get_Row (CD, 1));
   end Test_Get_Row;

end Char_Display_Driver.Tests;
