package body Surfaces is
   function Row_Luminosity
     (Surf : Surface_T; Row : Row_T) return Pixel_Component_Row_Array_T
   is
      --$ line answer
      Pix_Row : Pixel_Component_Row_Array_T (Surf'First (2) .. Surf'Last (2));
   begin
      --$ begin question
      -- Returns the luminosity of each pixel in the row so that it
      -- can later be used as a mean to convert it to a set of characters.
      -- For example a black pixel will be converted to '#' and a white pixel
      -- to ' ', giving "# # " for a checkerboard pattern.
      return (1 => <>); -- TODO
      --$ end question
      --$ begin answer
      for Col in Pix_Row'Range loop
         Pix_Row (Col) := Pixels.Luminosity (Surf (Row, Col));
      end loop;

      return Pix_Row;
      --$ end answer
   end Row_Luminosity;
end Surfaces;
