-----------------------------------------
--
--  Copyright (C) 2008-2010, AdaCore
--
-----------------------------------------

with Maze;
with Traversal;
with Pool_Manager;
with Search_Leads;
with Global_Options;

pragma Elaborate_All (Pool_Manager);

package Search_Team is

   procedure Launch_Searcher
     (Starting_Point : Maze.Position;
      Track          : Traversal.Trail;
      Launched       : out Boolean);
   --  Attempts to start a new searcher thread at the specified location and
   --  with the given trail. Launched will be True on return if a searcher was
   --  successfully launched, otherwise it will be False.

   procedure Await_Search_Complete;

private

   type Searcher;

   type Volunteer is access Searcher;

   task type Searcher is
      entry Start
        (My_ID : Volunteer;
         --  a pointer to this task, so it can put itself back into
         --  the pool
         Start : Maze.Position;
         --  the position to start from when searching within the maze
         Track : Traversal.Trail);
      --  the current, partial solution
   end Searcher;
   --  The task type representing the actual searcher threads. The searcher
   --  tasks wait for their next assignment (when taken from the pool), search
   --  as long as possible, and then put themselves back into the pool. While
   --  waiting to be taken from the pool they are also able to terminate.

   package Pool is new Pool_Manager
     (Pool_Size => Global_Options.Max_Searchers,
      Element   => Searcher,
      Reference => Volunteer);
   --  Searchers are maintained within a pool for the sake of avoiding the
   --  expense of repeatedly creating new threads. The number of threads in the
   --  pool is set by the global options, which come from the command line
   --  switches (with a default if not explicitly set). This number of threads
   --  is important, in that we should not have too few relative to the number
   --  of processors available. Although the unused tasks will be idle in the
   --  pool, there can be a not-insignificant overhead from having too many
   --  tasks when the number is quite large. Having the number set by parameter
   --  facilitates experimentation without requiring a rebuild of the
   --  application. The pool is loaded during package (body) elaboration.

   procedure Delegate
      (Next    : Maze.Positions;
       History : Traversal.Trail;
       Store   : in out Search_Leads.Repository);
   --  For each value in Next, a separate searcher thread is assigned that
   --  position and trail. If no searcher was available from the pool, the
   --  position and trail are saved in the repository Store so that the calling
   --  searcher thread can go back and pursue them later.

   procedure Pursue_Lead
     (Current_Position : in out Maze.Position;
      Path             : in out Traversal.Trail;
      Unsearched       : in out Search_Leads.Repository);
   --  Starting at Current_Position and Path within the maze, attempts to find
   --  the exit, updating Current_Position and Path along the way. Delegates
   --  other searcher threads when forks are encountered. When delegation is
   --  not possible it stores the unsearched Current_Position (and Path link)
   --  into Unsearched.

end Search_Team;
