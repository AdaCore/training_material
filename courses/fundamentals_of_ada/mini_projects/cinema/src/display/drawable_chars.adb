
package body Drawable_Chars is

   function Image (C : Drawable_Char_T) return String is
   begin
      return String (C);
   end Image;

   function Debug_Image (C : Sorted_Charset_T) return String is
   begin
      -- TODO
      return "";

   end Debug_Image;

   function Closest
     (Metric : Sorted_Charset_T; Value : Drawable_Char_Characteristic_T)
      return Drawable_Char_T
   is
   begin
      -- TODO
      return "";
   end Closest;

   function Sort_By
     (Charset        : Drawable_Charset_T;
      Characteristic : Drawable_Char_Caracteristics_List_T)
      return Sorted_Charset_T
   is
   begin
      return (others => <>);
   end Sort_By;

   function Reversed (SC : Sorted_Charset_T) return Sorted_Charset_T is
   begin
      return (others => <>);
   end Reversed;

end Drawable_Chars;
