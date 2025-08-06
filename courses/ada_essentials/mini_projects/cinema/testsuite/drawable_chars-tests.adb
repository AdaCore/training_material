with Ada.Wide_Wide_Text_IO;

package body Drawable_Chars.Tests is
   
   procedure Test_By_Black_Ratio (TC : Test_Case_T'Class) is
      C : Drawable_Charset_T := (+".", +" ");
      Charac : Drawable_Char_Caracteristics_List_T := (1, 0);
      SC : Sorted_Charset_T := Sort_By (C, Charac);
      use Chars_Sorted_By_Characteristic_Pkg;
      It : Cursor := SC.First;
   begin
      pragma Assert (Element (It).Char.all = " ");
      It := Next (It);
      pragma Assert (Element (It).Char.all = ".");
   end Test_By_Black_Ratio;
   
   procedure Test_Identical_Values (TC : Test_Case_T'Class) is
      By_Height : Sorted_Charset_T := Sort_By ((+"-", +"+"), (0, 0));
   begin
      null;
   end Test_Identical_Values;
   
   procedure Test_Closest_By_Height (TC : Test_Case_T'Class) is
      C : Drawable_Charset_T := (+".", +"-", +"_", +"'");
      Height : Drawable_Char_Caracteristics_List_T := (-1, 0, -5, 1);
      By_Height : Sorted_Charset_T := Sort_By (C, Height);
      use Chars_Sorted_By_Characteristic_Pkg;
   begin
      pragma Assert (Image (Closest (By_Height, 0)) = "-");
      pragma Assert (Image (Closest (By_Height, -2)) = ".");
      pragma Assert (Image (Closest (By_Height, -4)) = "_");
      pragma Assert (Image (Closest (By_Height, 2)) = "'");
   end Test_Closest_By_Height;

end Drawable_Chars.Tests;
