package Pixels is
   Pixel_Bits : constant Positive := 8;
   
   type Pixel_Component_T
      -- TODO: Declare integer 0 to 2^(Pixel_Bits)
      is new Integer;
   
   type Pixel_T is record
      -- TODO: R, G, B, A pixel components
      --      replace those dummy variables with the proper name and type
      DUMMY1, DUMMY2, DUMMY3, DUMMY4 : Integer;
   end record;
   
   function "+" (A, B : Pixel_T) return Pixel_T;
   function Luminosity (P : Pixel_T) return Pixel_Component_T;
   --  Returns the pixel luminosity as the mean of its components
end Pixels;
