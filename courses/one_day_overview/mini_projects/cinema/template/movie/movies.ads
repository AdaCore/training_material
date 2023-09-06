with Ada.Strings.Unbounded;
with Surfaces; use Surfaces;

package Movies is
   
   subtype Frame_T is Positive;
   -- Position of a frame in a movie
   
   type Resolution_T is record
      Rows : Row_T;
      Columns : Column_T;
   end record;
   -- Resolution of a movie
   
   type Movie_T is private;
   -- A movie is an ordered set of frames that are to be displayed in sequence
   -- On the file-system, movies are stored as directory that will contain
   -- several BMP called "XXX_0.bmp", "XXX_1.bmp"... where XXX can change
   Null_Movie : constant Movie_T;
   
   function Resolution (M : Movie_T) return Resolution_T;
   function File_Name (M : Movie_T) return String;
   -- Return the name of the movie's directory
   function Load_From (Path : String) return Movie_T;
   -- Load a movie from the given directory
   function Frames_Number (M : Movie_T) return Frame_T;
   -- Number of frames available in the given movie
   function Frame (M : Movie_T; F : Frame_T) return Surface_T
   with Pre => F <= Frames_Number (M);
   -- Pixel information for the given movie frame
   
private
   type Movie_T is record
      Path : Ada.Strings.Unbounded.Unbounded_String;
      Resolution : Resolution_T;
   end record;
   
   function Resolution (M : Movie_T) return Resolution_T is (M.Resolution);
   function File_Name (M : Movie_T) return String
   is (Ada.Strings.Unbounded.To_String (M.Path));
   
   Null_Movie : constant Movie_T
     := (Ada.Strings.Unbounded.Null_Unbounded_String,
         (Row_T'First, Column_T'First));
end Movies;
