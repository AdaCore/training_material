with Ada.Text_IO;

package body Char_Display_Driver is

   function Make
     (Surf : Surface_T; Lux_Charset : Sorted_Charset_T) return Char_Display
   is
   begin
      -- TODO
      return (others => <>);
   end Make;

   function Get_Row (CD : Char_Display; Row : Row_T) return String is
   begin
      -- TODO
      return "";
   end Get_Row;

   procedure Display_Row (CD : Char_Display; Row : Row_T) is
   begin
      -- TODO
      null;
   end Display_Row;

   procedure Display (CD : Char_Display) is
   begin
      -- Clear screen
      Ada.Text_IO.Put (Character'Val (8#33#) & "[2J");

      -- TODO
   end Display;

end Char_Display_Driver;
