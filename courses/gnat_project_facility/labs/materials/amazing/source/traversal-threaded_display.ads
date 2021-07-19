-----------------------------------------
--
--  Copyright (C) 2008-2010, AdaCore
--
-----------------------------------------
--
--  This is the thread-safe version of a facility for displaying maze
--  solutions.

package Traversal.Threaded_Display is

   procedure Show (Solution : Trail;  On : Maze.Reference);
   --  Displays the specified solution in a thread-safe manner if output is
   --  enabled. (The user might disable output for the sake of timing tests,
   --  for example.) Increments and displays the total number of solutions
   --  displayed so far.

private

   protected Mutex is
      entry Acquire;
      procedure Release;
   private
      Count : Natural := 1;
   end Mutex;
   --  Mutex is a lock used to acquire mutually exclusive access to the
   --  selected console so that the Show procedure is thread-safe.

   Total : Natural := 0;
   --  The total number of solutions displayed.  It is not incremented when the
   --  Display_Output option switch is set to false.

end Traversal.Threaded_Display;
