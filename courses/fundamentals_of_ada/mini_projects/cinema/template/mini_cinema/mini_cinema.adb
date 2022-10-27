--$ begin answer
with Ada.Directories;
with Ada.Text_IO;

--$ end answer
with Movie_Servers;
with Drawable_Chars;
with Drawable_Chars.Latin_1;
with Drawable_Chars.Circles;

procedure Mini_Cinema is
   type Charsets_List_T is
     array (Positive range <>) of Drawable_Chars.Sorted_Charset_T;

   Charsets : constant Charsets_List_T :=
     (Drawable_Chars.Circles.By_White, Drawable_Chars.Circles.By_Black,
      Drawable_Chars.Latin_1.By_White, Drawable_Chars.Latin_1.By_Black);

   --$ begin question
   -- TODO
   Play_Settings  : Movie_Servers.Play_Settings_Access_T := null;
   Display_Server : Movie_Servers.Movie_Server_Task_T (Play_Settings);
   --$ end question
   --$ begin answer
   Charset_Chosen : Positive := Positive'First;

   Play_Settings : constant Movie_Servers.Play_Settings_Access_T :=
     new Movie_Servers.Play_Settings_T;
   Display_Server : Movie_Servers.Movie_Server_Task_T (Play_Settings);
   --$ end answer
begin
   Play_Settings.Set_FPS (4);
   Play_Settings.Set_Charset (Charsets (Charsets'First));
   Display_Server.Play_Loop (Dir => "resources/movies/rotating_triangle");
   --$ begin question
   -- TODO
   -- Add commands for changing the server settings
   --$ end question
   --$ begin answer
   loop
      declare
         Input : constant String := Ada.Text_IO.Get_Line;
      begin
         if Input = "pause" then
            Display_Server.Pause;
         elsif Input = "resume" then
            Display_Server.Resume;
         elsif Input = "exit" then
            exit;
         elsif Input = "stop" then
            Display_Server.Stop;
         elsif Input = "char" then
            if Charset_Chosen = Charsets'Last then
               Charset_Chosen := Charsets'First;
            else
               Charset_Chosen := Charset_Chosen + 1;
            end if;
            Play_Settings.Set_Charset (Charsets (Charset_Chosen));
         elsif Input = "fast" then
            Play_Settings.Set_FPS (Play_Settings.FPS * 2);
         elsif Input = "slow" then
            if Play_Settings.FPS > 1 then
               Play_Settings.Set_FPS (Play_Settings.FPS / 2);
            end if;
         elsif Ada.Directories.Exists (Input) then
            Display_Server.Play_Loop (Input);
         end if;
      end;
   end loop;

   Display_Server.Finish;
   --$ end answer
end Mini_Cinema;
