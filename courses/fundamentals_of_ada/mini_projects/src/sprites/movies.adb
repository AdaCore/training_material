with Ada.Directories;       use Ada.Directories;
with Ada.Containers.Ordered_Sets;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with BMP_File_IO;

package body Movies is

   function Load_From (Path : String) return Movie_T is
   begin
      return (others => <>); -- TODO
   end Load_From;

   function Frames_Number (M : Movie_T) return Frame_T is
   begin
      return Frame_T'First; -- TODO
   end Frames_Number;

   -- TODO
   -- Use a Ada.Container to store all the files related to the movie
   -- (BMP files stored in the movie's directory).
   --
   -- Those files should be in order of their names, but be careful
   -- that a naive string sort would put "toto_9.bmp" > "toto_10.bmp"

   function Frame (M : Movie_T; F : Frame_T) return Surface_T is
   begin
      -- TODO
      -- To find *.bmp files, use the Ada.Directories search
      -- https://www.adaic.org/resources/add_content/standards/12rm/html/RM-A-16.html#p101

      return (others => <>);
   end Frame;

end Movies;
