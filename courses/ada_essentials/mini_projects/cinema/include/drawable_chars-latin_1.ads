package Drawable_Chars.Latin_1 is

   -- This package uses Latin_1 (actually ASCII subset) characters for rendering

   Chars : constant Drawable_Charset_T;
   By_Black : constant Sorted_Charset_T;
   By_White : constant Sorted_Charset_T;

private
   Chars : constant Drawable_Charset_T
     := (+"  ", +" .", +"..", +"--", +"~~", +"++", +"xx", +"XX", +"$$", +"##");

   By_Black : constant Sorted_Charset_T
     := Sort_By (Chars, (0, 8, 16, 32, 60, 90, 120, 154, 190, 220));
   By_White : constant Sorted_Charset_T
     := Reversed (By_Black);

end Drawable_Chars.Latin_1;
