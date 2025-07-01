with Char_Display_Driver; use Char_Display_Driver;
with Drawable_Chars.Latin_1;
with BMP_File_IO;

procedure Static_Image_Display is
   CD : Char_Display;
begin
   -- Basic
   CD := Make (Surf        => BMP_File_IO.Get ("resources/dummy_square_10x10.bmp"),
               Lux_Charset => Drawable_Chars.Latin_1.By_White);
   Display (CD);
   delay 1.0;

   -- Advanced: Check orientation and a larger size
   CD := Make (Surf        => BMP_File_IO.Get ("resources/dummy_triangle_20x20.bmp"),
               Lux_Charset => Drawable_Chars.Latin_1.By_White);
   Display (CD);
   delay 1.0;

   -- Expert: This is a 24 bits image created by Paint
   CD := Make (Surf        => BMP_File_IO.Get ("resources/sunset.bmp"),
               Lux_Charset => Drawable_Chars.Latin_1.By_White);
   Display (CD);
end Static_Image_Display;
