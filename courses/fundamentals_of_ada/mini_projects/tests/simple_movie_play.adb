with Movies; use Movies;
with Char_Display_Driver;
use all type Char_Display_Driver.Char_Display;
with Drawable_Chars.Latin_1;
with Ada.Calendar;
use type Ada.Calendar.Time;

procedure Simple_Movie_Play is
   M : Movie_T := Load_From ("resources/movies/rotating_triangle");
   F : Frame_T := Frame_T'First;
   Max_Frame : constant Frame_T := Frames_Number (M);
   Start_Time : Ada.Calendar.Time := Ada.Calendar.Clock;
   Next_Time : Ada.Calendar.Time := Ada.Calendar.Clock;
begin
   while Ada.Calendar.Clock - Start_Time < 6.0 loop
      Display
        (Make
           (Frame (M, F),
            Drawable_Chars.Latin_1.By_White));
      
      if F = Max_Frame then
         F := Frame_T'First;
      else
         F := Frame_T'Succ (F);
      end if;
      
      Next_Time := Next_Time + 1.0 / 4.0;
      
      delay until Next_Time;
   end loop;
end Simple_Movie_Play;
