package Pixels is
   Pixel_Bits : constant Positive := 8;
   
   type Pixel_Component_T
      is range 0 .. (2 ** Pixel_Bits) - 1;
   
   type Pixel_T is record
      R, G, B, A : Pixel_Component_T;
   end record;
   
   function "+" (A, B : Pixel_T) return Pixel_T;
   function Luminosity (P : Pixel_T) return Pixel_Component_T;
   --  Returns the pixel luminosity as the mean of its components
end Pixels;
