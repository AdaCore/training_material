with Surfaces; use Surfaces;
with Drawable_Chars; use Drawable_Chars;

package Char_Display_Driver is

   subtype Row_Display_T is Row_T range Row_T'First .. 60;
   subtype Column_Display_T is Column_T range Column_T'First .. 120;
   
   type Char_Display
     (Rows : Row_Display_T := Row_T'First;
      Columns : Column_Display_T := Column_T'First)
   is record
      Lux_Charset : Sorted_Charset_T;
      Surf : Surface_T (Row_T'First .. Rows, Column_T'First .. Columns);
   end record;
   --  A char display is a driver to display characters on the screen,
   --  given a sorted Lux_Charset, which will match luminosity values
   --  to strings, pixel-by-pixel.
   
   function Make (Surf : Surface_T; Lux_Charset : Sorted_Charset_T)
                  return Char_Display;
   -- Return a driver to draw the given surface with the given charset
   
   procedure Display_Row (CD : Char_Display; Row : Row_T)
   with Pre => Row in Row_T'First .. CD.Rows;
   -- Draw the given row

   procedure Display (CD : Char_Display);
   -- Draw the full surface
   
private
   function Get_Row (CD : Char_Display; Row : Row_T) return String;
   -- Return the drawn value for the given row, using the charset that
   -- has been set up.
   -- This function is made available there for testing and/or inheritance.

end Char_Display_Driver;
