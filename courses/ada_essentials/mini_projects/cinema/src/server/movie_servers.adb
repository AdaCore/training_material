with Ada.Exceptions;
with Movies;              use Movies;
with Char_Display_Driver; use Char_Display_Driver;
with Ada.Calendar;
use type Ada.Calendar.Time;
with Logs;
with Drawable_Chars.Latin_1;

package body Movie_Servers is

   protected body Play_Settings_T is
      function FPS return Positive is (FPS_Value);

      procedure Set_FPS (Value : Positive) is
      begin
         -- TODO
         null;
      end Set_FPS;

      function Charset return Sorted_Charset_T is (Charset_Value);

      procedure Set_Charset (Value : Sorted_Charset_T) is
      begin
         -- TODO
         null;
      end Set_Charset;

   end Play_Settings_T;

   function Make_Default return Movie_Server_Task_T is
   begin
      -- How to return a task there?
      return XXX; -- TODO
   end Make_Default;

   procedure Display_Frame
     (CD : in out Char_Display; Movie : Movie_T; Frame : Frame_T)
   is
   begin
      null; -- TODO
   end Display_Frame;

   task body Movie_Server_Task_T is
   begin
      --  TODO
      null;
   end Movie_Server_Task_T;

end Movie_Servers;
