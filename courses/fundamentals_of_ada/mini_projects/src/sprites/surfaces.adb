package body Surfaces is
   function Row_Luminosity
     (Surf : Surface_T; Row : Row_T) return Pixel_Component_Row_Array_T
   is
   begin
      return (1 => <>); -- TODO
   end Row_Luminosity;
end Surfaces;
