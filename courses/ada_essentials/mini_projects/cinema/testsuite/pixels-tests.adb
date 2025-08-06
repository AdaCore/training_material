package body Pixels.Tests is

   procedure Test_Add_Opaque (TC : Test_Case_T'Class) is
   begin
      pragma Assert
        (Pixel_T'(255, 0, 0, 255) + Pixel_T'(0, 0, 255, 255)
         = Pixel_T'(255, 0, 255, 255));
      pragma Assert
        (Pixel_T'(255, 255, 0, 255) + Pixel_T'(0, 0, 255, 255)
         = Pixel_T'(255, 255, 255, 255));
      pragma Assert
        (Pixel_T'(255, 255, 255, 255) + Pixel_T'(0, 0, 255, 255)
         = Pixel_T'(255, 255, 255, 255));
   end Test_Add_Opaque;

end Pixels.Tests;
