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
         --$ begin question
         -- TODO
         null;
         --$ end question
         --$ line answer
         FPS_Value := Value;
      end Set_FPS;

      function Charset return Sorted_Charset_T is (Charset_Value);

      procedure Set_Charset (Value : Sorted_Charset_T) is
      begin
         --$ begin question
         -- TODO
         null;
         --$ end question
         --$ line answer
         Charset_Value := Value;
      end Set_Charset;

   end Play_Settings_T;

   function Make_Default return Movie_Server_Task_T is
      --$ line answer
      S : constant Play_Settings_Access_T := new Play_Settings_T;
   begin
      --$ begin question
      -- How to return a task there?
      return XXX; -- TODO
      --$ end question
      --$ begin answer
      S.Set_FPS (4);
      S.Set_Charset (Drawable_Chars.Latin_1.By_Black);
      return T : Movie_Server_Task_T (Play_Settings => S);
      --$ end answer
   end Make_Default;

   procedure Display_Frame
     (CD : in out Char_Display; Movie : Movie_T; Frame : Frame_T)
   is
   begin
      --$ line question
      null; -- TODO
      --$ begin answer
      CD.Surf := Movies.Frame (Movie, Frame);
      Char_Display_Driver.Display (CD);
      --$ end answer
   end Display_Frame;

   task body Movie_Server_Task_T is
      --$ begin answer
      Playing      : Boolean                    := False;
      Frame        : Frame_T;
      End_Of_Times : constant Ada.Calendar.Time :=
        Ada.Calendar.Time_Of (Ada.Calendar.Year_Number'Last, 12, 31);
      Next_Frame_Time : Ada.Calendar.Time := End_Of_Times;
      Movie           : Movies.Movie_T    := Null_Movie;
      CD              : Char_Display;
      --$ end answer
   begin
      --$ begin question
      --  TODO
      null;
      --$ end question
      --$ begin answer
      loop
         select
            accept Play_Loop (Dir : String) do
               Movie := Movies.Load_From (Dir);
               declare
                  Res : constant Resolution_T := Resolution (Movie);
               begin
                  CD :=
                    (Rows => Res.Rows, Columns => Res.Columns, others => <>);
               end;
               Playing         := True;
               Frame           := Frame_T'First;
               Next_Frame_Time := Ada.Calendar.Clock;
            end Play_Loop;
         or
            accept Pause do
               Playing := False;
            end Pause;
         or
            accept Resume do
               Playing         := True;
               Next_Frame_Time := Ada.Calendar.Clock;
            end Resume;
         or
            accept Stop do
               Movie := Null_Movie;
            end Stop;
         or
            accept Finish;
            exit;
         or
            delay until Next_Frame_Time;
         end select;

         if Playing and Movie /= Null_Movie then
            CD.Lux_Charset := Play_Settings.Charset;
            Display_Frame (CD, Movie, Frame);
            Logs.Put_Line (File_Name (Movie));

            if Frame = Frames_Number (Movie) then
               Frame := Frame_T'First;
            else
               Frame := Frame_T'Succ (Frame);
            end if;

            Next_Frame_Time :=
              Next_Frame_Time + 1.0 / Duration (Play_Settings.FPS);
         else
            Next_Frame_Time := End_Of_Times;
         end if;
      end loop;
   exception
      when E : others =>
         Logs.Put (Sty => Logs.White_On_Red, S => " error ");
         Logs.Put_Line (" " & Ada.Exceptions.Exception_Name (E));
         Logs.Put_Line (Ada.Exceptions.Exception_Message (E));
         --$ end answer
   end Movie_Server_Task_T;

end Movie_Servers;
