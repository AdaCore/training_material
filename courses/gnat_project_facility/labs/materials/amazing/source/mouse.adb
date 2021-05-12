--
--            Copyright (C) 2008-2010, AdaCore
--
--  This program finds all the unique solutions in a given maze. It is intended
--  for mazes having more than one unique solution but will also solve those
--  with only one. A unique solution is one that has at least one location in
--  the maze that is different from all other solutions, without circularities.
--
--  This program uses a single thread ("task" in Ada) to find all the
--  solutions. Another version, named "mice", uses multiple threads. A
--  single-threaded version is useful for comparing the time required to solve
--  given mazes, particularly on uniprocessor and multiprocessor (or multicore)
--  machines.
--
--  The program will solve an existing maze if the name of the file containing
--  that maze is specified on the command line, alone. Alternatively, a new
--  maze is generated if the required height and width are specified on the
--  command line instead of a file name.
--
--  The program gives the user the option to save newly created mazes to a
--  file.
--
--  When generating a new maze, workable values for <width> and <height> will
--  depend on the operating system hosting the application. Both Windows and
--  any operating system supporting ANSI escape sequences are supported, but
--  the width and height allowed for ANSI terminals will vary by
--  implementation.
--
--  All the unique solutions are displayed at the end of the program, as well
--  as the total number of solutions, unless output is disabled (presumably for
--  the sake of timing analyses).

with Maze.Factory;
with Maze.IO;
with Console;
with Traversal.Display;
with Search_Leads;
with HCI;                    use HCI;
with GNAT.IO;                use GNAT.IO;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.IO_Exceptions;

with Global_Options;  use Global_Options;

procedure Mouse is

   Path             : Traversal.Trail := Traversal.Origin;
   Current_Position : Maze.Position;
   Unsearched       : Search_Leads.Repository;
   The_Maze         : constant Maze.Reference := Maze.Instance;

   procedure Pursue_Lead is
      Candidates     : Maze.Positions (Maze.Possible_Moves);
      Num_Candidates : Maze.Moves;
      use type Maze.Moves;
   begin
      loop
         Traversal.Mark
           (Current_Position, Path, The_Maze, Candidates, Num_Candidates);

         exit when The_Maze.At_Exit (Current_Position) or Num_Candidates = 0;

         for K in Candidates'First .. Num_Candidates - 1 loop
            Unsearched.Save (Candidates (K), Path);
         end loop;

         --  Pursue the remaining (or only) candidate
         Current_Position := Candidates (Num_Candidates);
      end loop;
   end Pursue_Lead;

begin
   if Global_Options.Arguments_Invalid then
      Show_Usage;
      return;
   end if;

   if Filename_Set then
      Maze.IO.Restore_Maze (To_String (File_Name));
   else
      Maze.Factory.Generate (Width, Height, Perfect);
   end if;

   if Display_Output then
      Console.Instance.Clear;
      Console.Instance.Draw (Maze.Instance);
   end if;

   Current_Position := The_Maze.Entrance;

   Searching : loop
      Pursue_Lead;

      if The_Maze.At_Exit (Current_Position) then
         Traversal.Display.Show (Path, On => The_Maze);
      end if;

      exit Searching when Unsearched.Empty;

      --  go back to a position encountered earlier at a fork
      Unsearched.Restore (Current_Position, Path);
   end loop Searching;

   Optionally_Save_Maze (not Filename_Set);
exception
   when Ada.IO_Exceptions.Name_Error =>
      Put_Line ("No file named '" & To_String (File_Name) & "' found.");
      Show_Usage;
end Mouse;
