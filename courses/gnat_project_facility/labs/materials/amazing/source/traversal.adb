-----------------------------------------
--
--  Copyright (C) 2008-2010, AdaCore
--
-----------------------------------------

with Console;

package body Traversal is

   ----------
   -- Mark --
   ----------

   procedure Mark
      (Point  :        Maze.Position;
       Past   : in out Trail;
       Within :        Maze.Reference;
       Next   :    out Maze.Positions;
       Last   :    out Maze.Moves)
   is
      Candidates : constant Maze.Positions := Maze.Next (Within, Point);
      Count      : Maze.Moves := 0;

      use type Maze.Moves;
   begin
      if Past.Solutions /= null then -- we've visited some locations
         --  take only candidates that we've not already visited
         for K in Candidates'Range loop
            if not Previously_Visited (Candidates (K), Past) then
               Count := Count + 1;
               Next (Count) := Candidates (K);
            end if;
         end loop;
         Last := Count;
      else -- none visited as yet so take all candidate positions
         Next (Candidates'Range) := Candidates;
         Last := Candidates'Last;
      end if;

      Past.Solutions := new Node'(Point, Past.Solutions);

      if Last > 1 then -- Point is an intersection
         Past.Intersections := new Node'(Point, Past.Intersections);
      end if;
   end Mark;

   ------------------------
   -- Previously_Visited --
   ------------------------

   function Previously_Visited (This : Maze.Position; Past : Trail)
      return Boolean
   is
      Predecessor : constant Maze.Position := Past.Solutions.Point;
      --  the position we just came from
      use type Maze.Position;
   begin
      return This = Predecessor or else
             Is_Member (This, Past.Intersections);
   end Previously_Visited;

   ---------------
   -- Is_Member --
   ---------------

   function Is_Member (This : Maze.Position;  Within : Link) return Boolean is
      Ptr : Link := Within;
      use type Maze.Position;
   begin
      while Ptr /= null and then Ptr.Point /= This loop
         Ptr := Ptr.Pred;
      end loop;
      return Ptr /= null;
   end Is_Member;

   ----------
   -- Plot --
   ----------

   procedure Plot (This : Trail) is
      Ptr : Link := This.Solutions;
      use Maze;
   begin
      while Ptr /= null loop
         if Is_Member (Ptr.Point, This.Intersections) then
            Console.Instance.Plot_Intersection_Point
              ((Row (Ptr.Point), Column (Ptr.Point)));
         else
            Console.Instance.Plot_Solution_Point
              ((Row (Ptr.Point), Column (Ptr.Point)));
         end if;
         Ptr := Ptr.Pred;
      end loop;
   end Plot;

end Traversal;
