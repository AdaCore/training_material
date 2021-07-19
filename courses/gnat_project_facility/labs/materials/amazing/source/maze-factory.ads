--
--            Copyright (C) 2008-2010, AdaCore
--

package Maze.Factory is

   procedure Generate
     (Width   : Positive;
      Height  : Positive;
      Perfect : Boolean := True);
   --  Generate a new maze, with the given Width and Height, setting the
   --  singleton maze representation accordingly. Note that "perfect" mazes
   --  have only one solution; others have an indefinite number greater than
   --  one.
   --
   --  We don't need a parameter for the maze itself because the current maze
   --  is represented as a singleton, hence the body of Generate can reference
   --  it as usual.

end Maze.Factory;
