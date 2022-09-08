with Ada.Directories;       use Ada.Directories;
with Ada.Containers.Ordered_Sets;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with BMP_File_IO;

package body Movies is

   function Load_From (Path : String) return Movie_T is
   begin
      --$ line question
      return (others => <>); -- TODO
      --$ begin answer
      pragma Assert (Exists (Path));

      declare
         Movie : Movie_T := (Path => To_Unbounded_String (Path), others => <>);
         Surface : Surface_T := Frame (Movie, Frame_T'First);
      begin
         Movie.Resolution := (Surface'Length (1), Surface'Length (2));
         return Movie;
      end;
      --$ end answer
   end Load_From;

   function Frames_Number (M : Movie_T) return Frame_T is
      --$ begin answer
      S : Search_Type;
      E : Directory_Entry_Type;
      N : Natural := 0;
      --$ end answer
   begin
      --$ line question
      return Frame_T'First; -- TODO
      --$ begin answer
      Start_Search (S, To_String (M.Path), "*.bmp");

      while More_Entries (S) loop
         Get_Next_Entry (S, E);
         N := N + 1;
      end loop;

      return N;
      --$ end answer
   end Frames_Number;

   --$ begin question
   -- TODO
   -- Use a Ada.Container to store all the files related to the movie
   -- (BMP files stored in the movie's directory).
   --
   -- Those files should be in order of their names, but be careful
   -- that a naive string sort would put "toto_9.bmp" > "toto_10.bmp"
   --$ end question
   --$ begin answer
   function Numerical_LT (A, B : Unbounded_String) return Boolean is
   -- Sort files, taking into account a numerical suffix
   begin
      if Length (A) /= Length (B) then
         return Length (A) < Length (B);
      else
         return A < B;
      end if;
   end Numerical_LT;

   package String_Sorted_Numerically_Set_Pkg is new Ada.Containers.Ordered_Sets
     (Element_Type => Unbounded_String, "<" => Numerical_LT);
   --$ end answer

   function Frame (M : Movie_T; F : Frame_T) return Surface_T is
      --$ begin answer
      S : Search_Type;
      E : Directory_Entry_Type;
      N : Frame_T := 1;
      use String_Sorted_Numerically_Set_Pkg;
      All_Files : String_Sorted_Numerically_Set_Pkg.Set;
      --$ end answer
   begin
      --$ begin question
      -- TODO
      -- To find *.bmp files, use the Ada.Directories search
      -- https://www.adaic.org/resources/add_content/standards/12rm/html/RM-A-16.html#p101

      return (others => <>);
      --$ end question
      --$ begin answer
      Start_Search (S, To_String (M.Path), "*.bmp");
      while More_Entries (S) loop
         Get_Next_Entry (S, E);
         Insert (All_Files, To_Unbounded_String (Simple_Name (E)));
      end loop;

      for Name of All_Files loop
         if N = F then
            declare
               F : File_Type;
            begin
               Open
                 (File => F, Mode => In_File,
                  Name => To_String (M.Path & "/" & Name));
               declare
                  Surf : Surface_T := BMP_File_IO.Get (F);
               begin
                  Close (F);
                  return Surf;
               exception
                  when others =>
                     Close (F);
                     raise;
               end;
            end;
         end if;

         N := N + 1;
      end loop;
      raise Program_Error;
      --$ end answer
   end Frame;

end Movies;
