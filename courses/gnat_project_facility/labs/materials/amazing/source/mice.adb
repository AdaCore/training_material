--
--            Copyright (C) 2008-2010, AdaCore
--
--  This is a multi-threaded program that finds all the unique solutions in a
--  given maze. It is intended for mazes having more than one unique solution
--  but will also solve those with only one. A unique solution is one that has
--  at least one location in the maze that is different from all other
--  solutions, without circularities.
--
--  The program finds all the solutions to the specified maze by flooding the
--  maze with searcher threads ("tasks" in Ada). Whenever a searcher encounters
--  a "fork" in the maze path, where more than one maze location can be visited
--  next, it delegates those locations to other searchers, taking one for
--  itself. The program is, therefore, best suited for computers with multiple
--  processors (or multicores) but will run on uniprocessors too.
--
--  The program will solve an existing maze if the name of the file containing
--  that maze is specified on the command line, alone. Alternatively, a new
--  maze is generated if the required height and width are specified on the
--  command line instead of a file name.
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
with Search_Team;
with Traversal;
with HCI;                    use HCI;
with Global_Options;         use Global_Options;
with GNAT.IO;                use GNAT.IO;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.IO_Exceptions;

procedure Mice is

   Successful_Launch : Boolean;
   pragma Unreferenced (Successful_Launch);
   --  We know the pool initially has at least one member and this is the first
   --  request from the pool, so no need to check Successful_Launch.

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

   Search_Team.Launch_Searcher
     (Starting_Point => Maze.Instance.Entrance,
      Track          => Traversal.Origin,
      Launched       => Successful_Launch);

   Search_Team.Await_Search_Complete;

   Optionally_Save_Maze (not Filename_Set);
exception
   when Ada.IO_Exceptions.Name_Error =>
      Put_Line ("No file named '" & To_String (File_Name) & "' found.");
      Show_Usage;
end Mice;
