package body Surfaces is
   function Row_Luminosity
     (Surf : Surface_T; Row : Row_T) return Pixel_Component_Row_Array_T
   is
      Pix_Row : Pixel_Component_Row_Array_T (Surf'First (2) .. Surf'Last (2));
   begin
      for Col in Pix_Row'Range loop
         Pix_Row (Col) := Pixels.Luminosity (Surf (Row, Col));
      end loop;

      return Pix_Row;
   end Row_Luminosity;
end Surfaces;
