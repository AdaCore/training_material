with Pixels;

package Surfaces is

   type Row_T is new Positive;
   type Column_T is new Positive;
   
   type Surface_T is array (Row_T range <>, Column_T range <>) of Pixels.Pixel_T;
   
   type Pixel_Component_Row_Array_T is array (Column_T range <>) of Pixels.Pixel_Component_T;
   
   function Row_Luminosity (Surf : Surface_T; Row : Row_T)
     return Pixel_Component_Row_Array_T
     with Pre => Row <= Surf'Last (1),
          Post => Row_Luminosity'Result'Length = Surf'Length (2);
   -- Calculate luminosity for all of the given row's pixels.

end Surfaces;
