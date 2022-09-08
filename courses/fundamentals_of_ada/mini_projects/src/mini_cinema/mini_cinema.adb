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

   -- TODO
   Play_Settings  : Movie_Servers.Play_Settings_Access_T := null;
   Display_Server : Movie_Servers.Movie_Server_Task_T (Play_Settings);
begin
   Play_Settings.Set_FPS (4);
   Play_Settings.Set_Charset (Charsets (Charsets'First));
   Display_Server.Play_Loop (Dir => "resources/movies/rotating_triangle");
   -- TODO
   -- Add commands for changing the server settings
end Mini_Cinema;
