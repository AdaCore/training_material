--
--            Copyright (C) 2008-2010, AdaCore
--

with Ada.Numerics.Float_Random;
with Unbounded_Sequential_Stacks;

package body Maze.Factory is

   subtype Possible_Neighbors is Integer range 1 .. 4;
   --  we can only go north, south, east, or west, ie not diagonally,
   --  so a cell can have at most 4 neighbors

   type Neighbor_List is array (Possible_Neighbors range <>) of Position;

   function Intact_Cell (P : Position; The_Maze : Reference) return Boolean;
   --  Returns whether all the walls of position P within The_Maze are present

   function Intact_Neighbors (This : Position; The_Maze : Reference)
      return Neighbor_List;
   --  Returns a list of positions adjacent to This within The_Maze in
   --  which all the walls of each position are present

   package Positions is new Unbounded_Sequential_Stacks (Position);
   use Positions;

   function Random_Constrained (By : Positive) return Positive;
   --  Returns a pseudo-random number in the range 1 .. By

   --------------
   -- Generate --
   --------------

   procedure Generate
      (Width   : Positive;
       Height  : Positive;
       Perfect : Boolean := True)
   is
      The_Maze : constant Reference := Maze.Instance;
      --  The maze is a singleton so we get a reference to it here, but as a
      --  singleton it is a shared reference so there is no issue of working on
      --  the "wrong" copy.

      subtype Possible_Rows is Integer range 1 .. Height;
      subtype Possible_Cols is Integer range 1 .. Width;

      Total_Cells       : Integer;
      Current           : Position;
      Visited_Cells     : Positive := 1;
      Visited_Positions : Positions.Stack;
   begin
      The_Maze.Grid := new Maze_Representation (Possible_Rows, Possible_Cols);

      The_Maze.Actual_Rows := Height;
      The_Maze.Actual_Columns := Width;

      Total_Cells := Width * Height;

      --  We start at a random position within the maze

      Current := Position'(Row    => Random_Constrained (By => Height),
                           Column => Random_Constrained (By => Width));

      while Visited_Cells < Total_Cells loop
         declare
            Neighbors : constant Neighbor_List :=
               Intact_Neighbors (Current, The_Maze);
            Index     : Possible_Neighbors;
         begin
            if Neighbors'Length > 0 then
               Index := Random_Constrained (By => Neighbors'Length);
               Delete_Wall_Between (The_Maze, Current, Neighbors (Index));
               Push (Visited_Positions, Current);
               Current := Neighbors (Index);
               Visited_Cells := Visited_Cells + 1;
            else
               Pop (Visited_Positions, Current);
            end if;
         end;
      end loop;

      --  We arbitrarily set the entrance and exit at the opposite corners

      The_Maze.Start_Point := (Possible_Rows'First, Possible_Cols'First);
      The_Maze.End_Point   := (Possible_Rows'Last,  Possible_Cols'Last);

      --  Remove the necessary walls to reflect the entrance and exit positions

      declare
         The_Entrance : Cell renames The_Maze.Grid
            (The_Maze.Start_Point.Row, The_Maze.Start_Point.Column);
         The_Exit : Cell renames The_Maze.Grid
            (The_Maze.End_Point.Row, The_Maze.End_Point.Column);
      begin
         The_Entrance.Walls (West) := False;
         The_Exit.Walls (East) := False;
      end;

      --  If the maze is not intended to be "perfect", ie to have only one
      --  solution, we remove some walls so that there will be multiple
      --  solutions. We do this by walking down the diagonal, removing the wall
      --  between the cell on the diagonal and the one to the right of it
      --  (except for the last one which has no neighbor to the right).

      if not Perfect then
         for R in Possible_Rows loop
            for C in Possible_Cols loop
               if R = C and C /= Possible_Cols'Last then
                  Delete_Wall_Between (The_Maze, (R, C), (R, C + 1));
               end if;
            end loop;
         end loop;
      end if;
   end Generate;

   ----------------------
   -- Intact_Neighbors --
   ----------------------

   function Intact_Neighbors (This : Position; The_Maze : Reference)
      return Neighbor_List
   is
      Result : Neighbor_List (Possible_Neighbors);
      Count  : Integer range 0 .. Possible_Neighbors'Last := 0;
   begin
      for K in Offsets'Range loop
         declare
            Candidate : constant Position := This + Offsets (K);
         begin
            if On_Grid (The_Maze, Candidate) then
               if Intact_Cell (Candidate, The_Maze) then
                  Count := Count + 1;
                  Result (Count) := Candidate;
               end if;
            end if;
         end;
      end loop;
      return Result (1 .. Count);
   end Intact_Neighbors;

   All_Walls_Present : constant Wall_Flags := (others => True);
   --  A constant indicating that all four walls are present

   -----------------
   -- Intact_Cell --
   -----------------

   function Intact_Cell (P : Position; The_Maze : Reference) return Boolean is
      This_Cell : Cell renames The_Maze.Grid (P.Row, P.Column);
   begin
      return This_Cell.Walls = All_Walls_Present;
   end Intact_Cell;

   use Ada.Numerics.Float_Random;

   G : Generator;
   --  The generator used by Random_Constrained

   ------------------------
   -- Random_Constrained --
   ------------------------

   function Random_Constrained (By : Positive) return Positive is
      Result : Integer;
      F      : Long_Float;
      R      : constant Float := Random (G); -- in the range 0.0 .. 1.0
   begin
      F := Long_Float (By) * Long_Float (R);
      Result := Integer (F);
      if Result < 1 then
         Result := 1;
      end if;
      return Result;
   end Random_Constrained;

begin
   Reset (G);
end Maze.Factory;
