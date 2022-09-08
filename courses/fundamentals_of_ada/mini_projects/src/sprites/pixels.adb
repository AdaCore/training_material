package body Pixels is

   function "+" (A, B : Pixel_T) return Pixel_T is
   begin
      return (others => 0); -- TODO
   end "+";

   function Luminosity (P : Pixel_T) return Pixel_Component_T is
   begin
      return 0; -- TODO
   end Luminosity;

end Pixels;
