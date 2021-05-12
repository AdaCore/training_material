--
--            Copyright (C) 2008-2010, AdaCore
--

with Ada.Unchecked_Deallocation;
with Ada.Streams.Stream_IO;

package body Maze.IO is

   ---------------
   -- Save_Maze --
   ---------------

   procedure Save_Maze (File_Name : String) is
      use Ada.Streams.Stream_IO;

      File     : File_Type;
      The_Maze : constant Reference := Instance;
   begin
      Create (File, Out_File, File_Name);

      Position'Write (Stream (File), The_Maze.Start_Point);
      Position'Write (Stream (File), The_Maze.End_Point);

      Integer'Write (Stream (File), The_Maze.Actual_Rows);
      Integer'Write (Stream (File), The_Maze.Actual_Columns);

      for R in 1 .. The_Maze.Actual_Rows loop
         for C in 1 .. The_Maze.Actual_Columns loop
            Cell'Write (Stream (File), The_Maze.Grid (R, C));
         end loop;
      end loop;

      Close (File);
   end Save_Maze;

   ----------
   -- Free --
   ----------

   procedure Free is
      new Ada.Unchecked_Deallocation (Object => Maze_Representation,
                                      Name   => Layout);

   ------------------
   -- Restore_Maze --
   ------------------

   procedure Restore_Maze (File_Name : String) is
      use Ada.Streams.Stream_IO;

      The_Maze : constant Reference := Instance;

      File : File_Type;
   begin
      Open (File, In_File, File_Name);

      Position'Read (Stream (File), The_Maze.Start_Point);
      Position'Read (Stream (File), The_Maze.End_Point);

      Integer'Read (Stream (File), The_Maze.Actual_Rows);
      Integer'Read (Stream (File), The_Maze.Actual_Columns);

      Free (The_Maze.Grid);

      The_Maze.Grid := new Maze_Representation
         (1 .. The_Maze.Actual_Rows, 1 .. The_Maze.Actual_Columns);

      for R in 1 .. The_Maze.Actual_Rows loop
         for C in 1 .. The_Maze.Actual_Columns loop
            Cell'Read (Stream (File), The_Maze.Grid (R, C));
         end loop;
      end loop;

      Close (File);
   end Restore_Maze;

end Maze.IO;
