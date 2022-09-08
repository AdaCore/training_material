package Drawable_Chars.Circles is

   -- This package uses circles of various size for rendering

   Chars : constant Drawable_Charset_T;
   By_Black : constant Sorted_Charset_T;
   By_White : constant Sorted_Charset_T;

private
   Chars : constant Drawable_Charset_T
     := (+"  ", +" ⋅", +"⋅⋅", +"⋅∙", +"∙∙", +"∙●", +"●●");

   By_Black : constant Sorted_Charset_T
     := Sort_By (Chars, (0, 8, 16, 24, 64, 130, 200));
   By_White : constant Sorted_Charset_T
     := Reversed (By_Black);
   

end Drawable_Chars.Circles;
