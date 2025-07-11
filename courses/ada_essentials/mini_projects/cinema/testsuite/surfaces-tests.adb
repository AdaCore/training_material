with Pixels;
use type Pixels.Pixel_Component_T;
package body Surfaces.Tests is

   procedure Test_Full_White_Row_Luminosity (TC : Test_Case_T'Class) is
      White : Pixels.Pixel_T := (255, 255, 255, 255);
      Surf : Surface_T := (1 => (White, White, White, White));
   begin
      pragma Assert (Row_Luminosity (Surf, 1) = (255, 255, 255, 255));
   end Test_Full_White_Row_Luminosity;

   procedure Test_Full_Black_Row_Luminosity (TC : Test_Case_T'Class) is
      Black : Pixels.Pixel_T := (0, 0, 0, 255);
      Surf : Surface_T := (1 => (Black, Black, Black, Black));
   begin
      pragma Assert (Row_Luminosity (Surf, 1) = (0, 0, 0, 0));
   end Test_Full_Black_Row_Luminosity;

   procedure Test_Gray_Row_Luminosity (TC : Test_Case_T'Class) is
      Gray : Pixels.Pixel_T := (255, 255, 255, 128);
      Surf : Surface_T := (1 => (Gray, Gray, Gray, Gray));
   begin
      pragma Assert (Row_Luminosity (Surf, 1) = (128, 128, 128, 128));
   end Test_Gray_Row_Luminosity;

end Surfaces.Tests;
