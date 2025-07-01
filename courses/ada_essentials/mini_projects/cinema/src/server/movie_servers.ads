with Drawable_Chars; use Drawable_Chars;

package Movie_Servers is
   
   protected type Play_Settings_T is
      function FPS return Positive;
      procedure Set_FPS (Value : Positive);

      function Charset return Sorted_Charset_T; 
      procedure Set_Charset (Value : Sorted_Charset_T);

   private
      FPS_Value : Positive;
      -- Frames-per-second for rendering
      Charset_Value : Sorted_Charset_T;
      -- Charset used for rendering
   end Play_Settings_T;
   -- Playback information for a movie
   
   type Play_Settings_Access_T is access all Play_Settings_T;

   task type Movie_Server_Task_T (Play_Settings : Play_Settings_Access_T) is
      entry Play_Loop (Dir : String);
      -- Start playing the movie in a neverending loop
      entry Pause;
      -- Pause the movie at its current frame
      entry Resume;
      -- Resume playing the movie
      entry Stop;
      -- Stop the movie, it cannot be resumed
      entry Finish;
      -- Stop the task and exit it
   end Movie_Server_Task_T;
   -- Task to play a movie with the given playback settings
   
   function Make_Default return Movie_Server_Task_T;
   -- Constructor for Movie_Server_Task_T, with default playback settings

end Movie_Servers;
