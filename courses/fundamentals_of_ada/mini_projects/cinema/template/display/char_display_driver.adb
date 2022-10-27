with Ada.Text_IO;
--$ line answer
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Char_Display_Driver is

   function Make
     (Surf : Surface_T; Lux_Charset : Sorted_Charset_T) return Char_Display
   is
   begin
      --$ begin question
      -- TODO
      return (others => <>);
      --$ end question
      --$ begin answer
      return
        Char_Display'
          (Rows => Surf'Length (1), Columns => Surf'Length (2), Surf => Surf,
           Lux_Charset => Lux_Charset);
      --$ end answer
   end Make;

   function Get_Row (CD : Char_Display; Row : Row_T) return String is
      --$ begin answer
      Lux : constant  Pixel_Component_Row_Array_T := Row_Luminosity (CD.Surf, Row);
      S   : Unbounded_String;
      --$ end answer
   begin
      --$ begin question
      -- TODO
      return "";
      --$ end question
      --$ begin answer
      for L of Lux loop
         Append
           (S,
            Image
              (Closest (CD.Lux_Charset, Drawable_Char_Characteristic_T (L))));
      end loop;

      return To_String (S);
      --$ end answer
   end Get_Row;

   procedure Display_Row (CD : Char_Display; Row : Row_T) is
   begin
      --$ begin question
      -- TODO
      null;
      --$ end question
      --$ line answer
      Ada.Text_IO.Put_Line (Get_Row (CD, Row));
   end Display_Row;

   procedure Display (CD : Char_Display) is
   begin
      -- Clear screen
      Ada.Text_IO.Put (Character'Val (8#33#) & "[2J");

      --$ line question
      -- TODO
      --$ begin answer
      for Row in CD.Surf'Range (1) loop
         Display_Row (CD, Row);
      end loop;
      --$ end answer
   end Display;

end Char_Display_Driver;
