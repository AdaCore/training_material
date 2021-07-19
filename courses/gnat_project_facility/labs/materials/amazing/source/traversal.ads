--
--            Copyright (C) 2008-2010, AdaCore
--

with Maze;

package Traversal is

   type Trail is private;
   --  A Trail represents a single solution path within a maze.

   Origin : constant Trail;

   procedure Mark (Point  :        Maze.Position;
                   Past   : in out Trail;
                   Within :        Maze.Reference;
                   Next   :    out Maze.Positions;
                   Last   :    out Maze.Moves);
   --  Adds position Point to the solution trail Past.
   --  Returns in Next all the positions that can be visited from that Point on
   --  the maze Within, if any, and indicates the Last index of Next that was
   --  assigned a value. Last might be zero, as when a dead-end is encountered
   --  at Point or Point is the exit. Next will not include the position we
   --  just came from, thereby preventing solution path circularities.

private

   type Node;

   type Link is access Node;

   type Node is
      record
         Point : Maze.Position;
         Pred  : Link;
      end record;

   type Trail is
      record
         Intersections : Link;
         Solutions     : Link;
      end record;
   --  A trail is actually a tuple consisting of a list of solution positions
   --  paired with a list of intersections included in that solution. All the
   --  positions in a given maze solution are included in the Solutions list.
   --  Intersections are positions from which more than one subsequent position
   --  can be visited, i.e., intersections are forks in a path. All positions
   --  in Intersections are also in Solutions.
   --
   --  We use a linked data structure because it is easily shared and updated
   --  by the separate threads without requiring mutually exclusive access and
   --  also because it directly represents solutions with forks. In fact, a
   --  Solutions list is really a tree of position nodes, any one of which,
   --  when starting at a bottom node, leads all the way back up to the
   --  entrance of the maze. The entrance to the maze is always the root node
   --  in the tree. Sharing is facilitated by the fact that any fork in a path
   --  just means that a position node has more than one successor node
   --  pointing back to it.
   --
   --  For example, imagine that the maze entrance position has adjacent
   --  positions with walls open such that two positions could be visited next.
   --  In other words, the entrance has an immediate fork in the path. The
   --  content of Solutions would look like the following, in which two
   --  successor nodes, rather than one, would designate the root entrance
   --  node. Each of those branches would eventually end with a node containing
   --  the maze exit position and each represents a distinct solution to the
   --  maze.
   --
   --   +------+----------+       +------+-------+            +------+------+
   --   | null | entrance | <-----| pred | point | ...  <-----| pred | exit |
   --   +------+----------+       +------+-------+            +------+------+
   --             ^
   --             |
   --             |               +------+-------+            +------+------+
   --             ----------------| pred | point | ...  <-----| pred | exit |
   --                             +------+-------+            +------+------+
   --
   --  Any given searcher thread has one solution branch. When there is more
   --  than one solution, some of the nodes are shared, as depicted above in
   --  which the entrance node is shared. Sharing is inexpensive across threads
   --  because the common upper branches of a solution need not be locked in
   --  any way; the searcher task just creates a new node designating the
   --  shared predecessor node.
   --
   --  Note that an effect of this approach is that a solution can only be
   --  displayed in reverse, from the exit back to the entrance (absent any
   --  additional processing), because the most current node/position is that
   --  of the exit and the tree leads back up to the entrance.
   --
   --  The list of intersections is used to detect and prevent circular
   --  solution paths. When determining if a given position has already been
   --  visited, instead of examining every position in the solution we can just
   --  examine the intersections because positions between intersections are
   --  not of interest. Essentially it is an optimized representation of all
   --  the positions visited by the Solutions list.

   Origin : constant Trail := (null, null);

   function Previously_Visited (This : Maze.Position; Past : Trail)
      return Boolean;
   --  Prevents circularities by answering "Have we been there before?"

   function Is_Member (This : Maze.Position;  Within : Link) return Boolean;
   --  Returns whether This occurs within the list of nodes designated by
   --  Within.

   procedure Plot (This : Trail);
   --  Displays a given solution trail on the selected console.  This is not a
   --  thread-safe routine and is therefore not called directly by threads.

end Traversal;
