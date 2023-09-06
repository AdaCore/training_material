package body Surfaces is
   function Row_Luminosity
     (Surf : Surface_T; Row : Row_T) return Pixel_Component_Row_Array_T
   is
   begin
      -- Returns the luminosity of each pixel in the row so that it
      -- can later be used as a mean to convert it to a set of characters.
      -- For example a black pixel will be converted to '#' and a white pixel
      -- to ' ', giving "# # " for a checkerboard pattern.
      return (1 => <>); -- TODO
   end Row_Luminosity;
end Surfaces;
