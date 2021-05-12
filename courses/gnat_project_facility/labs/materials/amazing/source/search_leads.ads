-----------------------------------------
--
--  Copyright (C) 2008-2010, AdaCore
--
-----------------------------------------

with Unbounded_Sequential_Queues;
with Maze;
with Traversal;

package Search_Leads is

   type Repository is tagged limited private;
   --  A Repository holds position and trail tuples, each of which represents a
   --  "search lead" in that a searcher task can go back to that position and
   --  point on the trail to search for a solution from that point forward to
   --  the maze exit.

   procedure Save (Into : in out Repository;
                   Pos  : Maze.Position;
                   Path : Traversal.Trail);
   --  Stores a position and trail pair into the repository.

   procedure Restore (From : in out Repository;
                      Pos  :    out Maze.Position;
                      Path :    out Traversal.Trail);
   --  Restores a position and trail pair from the repository. The order of
   --  pairs obtained from a repository is not specified.

   function Empty (This : Repository) return Boolean;
   --  Returns whether the repository contains any more search lead pairs.

private

   type Search_Info is
      record
         Pos  : Maze.Position;
         Path : Traversal.Trail;
      end record;

   package Tips is new Unbounded_Sequential_Queues (Search_Info);

   type Repository is new Tips.Queue with null record;
   --  We use a simple queue to represent the repository, which is OK because
   --  no order is specified or required.

end Search_Leads;
