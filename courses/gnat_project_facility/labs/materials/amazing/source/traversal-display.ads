-----------------------------------------
--
--  Copyright (C) 2008-2010, AdaCore
--
-----------------------------------------
--
--  This is a sequential, ie non-thread-safe version for use by single-
--  threaded maze solvers.

package Traversal.Display is

   procedure Show (Solution : Trail;  On : Maze.Reference);
   --  Displays the specified solution in a non-thread-safe manner, if output
   --  is enabled. (The user might disable output for the sake of timing tests,
   --  for example.) Increments and displays the total number of solutions
   --  displayed so far.

end Traversal.Display;
