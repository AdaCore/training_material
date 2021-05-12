-----------------------------------------
--
--  Copyright (C) 2008-2010, AdaCore
--
-----------------------------------------

with Traversal.Threaded_Display;

package body Search_Team is

   ---------------------
   -- Launch_Searcher --
   ---------------------

   procedure Launch_Searcher
     (Starting_Point : Maze.Position;
      Track          : Traversal.Trail;
      Launched       : out Boolean)
   is
      Member : Volunteer;
   begin
      Member := Pool.Next_Member (Wait => False);
      if Member /= null then
         Member.Start (Member, Starting_Point, Track);
         Launched := True;
      else
         Launched := False;
      end if;
   end Launch_Searcher;

   --------------
   -- Delegate --
   --------------

   procedure Delegate
      (Next    : Maze.Positions;
       History : Traversal.Trail;
       Store   : in out Search_Leads.Repository)
   is
      Success : Boolean;
   begin
      for Move in Next'Range loop
         Launch_Searcher (Starting_Point => Next (Move),
                          Track          => History,
                          Launched       => Success);
         if not Success then  --  none available now
            Store.Save (Next (Move), History);
         end if;
      end loop;
   end Delegate;

   --------------
   -- Searcher --
   --------------

   task body Searcher is
      Path             : Traversal.Trail;
      The_Maze         : constant Maze.Reference := Maze.Instance;
      Current_Position : Maze.Position;
      Myself           : Volunteer;
      Unsearched       : Search_Leads.Repository;
   begin
      loop
         select
            accept Start (My_ID : Volunteer;
                          Start : Maze.Position;
                          Track : Traversal.Trail)
            do
               Myself := My_ID;
               Current_Position := Start;
               Path := Track;
            end Start;
         or
            terminate;
         end select;

         Searching : loop
            Pursue_Lead (Current_Position, Path, Unsearched);

            if The_Maze.At_Exit (Current_Position) then
               Traversal.Threaded_Display.Show (Path, On => The_Maze);
            end if;

            exit Searching when Unsearched.Empty;

            --  go back to a position encountered earlier (at a fork) that
            --  could not be delegated at the time
            Unsearched.Restore (Current_Position, Path);
         end loop Searching;

         Pool.Return_Member (Myself);
      end loop;
   end Searcher;

   -----------------
   -- Pursue_Lead --
   -----------------

   procedure Pursue_Lead
     (Current_Position : in out Maze.Position;
      Path             : in out Traversal.Trail;
      Unsearched       : in out Search_Leads.Repository)
   is
      Candidates     : Maze.Positions (Maze.Possible_Moves);
      Num_Candidates : Maze.Moves;
      The_Maze       : constant Maze.Reference := Maze.Instance;
      use type Maze.Moves;
   begin
      loop
         Traversal.Mark
           (Current_Position, Path, The_Maze, Candidates, Num_Candidates);

         exit when The_Maze.At_Exit (Current_Position) or Num_Candidates = 0;

         Delegate (Candidates (Candidates'First .. Num_Candidates - 1),
                   Path,
                   Unsearched);

         --  Pursue the remaining (or only) candidate
         Current_Position := Candidates (Num_Candidates);
      end loop;
   end Pursue_Lead;

   ---------------------------
   -- Await_Search_Complete --
   ---------------------------

   procedure Await_Search_Complete is
   begin
      Pool.Await_Quiescence;
   end Await_Search_Complete;

--  Load the pool during package elaboration with the maximum number of
--  searchers specified.
begin
   Pool.Load (Pool.Initial_Values'(others => new Searcher));
end Search_Team;
