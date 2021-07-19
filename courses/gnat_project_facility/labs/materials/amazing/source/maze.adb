--
--            Copyright (C) 2008-2010, AdaCore
--

package body Maze is

   --------------
   -- Instance --
   --------------

   function Instance return Reference is
   begin
      return The_Instance;
   end Instance;

   ---------
   -- "+" --
   ---------

   function "+" (Left : Position; Right : Offset) return Position is
   begin
      return Position'(Left.Row + Right.Row, Left.Column + Right.Column);
   end "+";

   -------------
   -- On_Grid --
   -------------

   function On_Grid (This : access Puzzle;  Proposed : Position)
      return Boolean
   is
   begin
      return (Proposed.Row    in 1 .. This.Actual_Rows and
              Proposed.Column in 1 .. This.Actual_Columns);
   end On_Grid;

   ----------------
   -- Acceptable --
   ----------------

   function Acceptable (This : access Puzzle;  Proposed, Current : Position)
      return Boolean
   is
   begin
      return On_Grid (This, Proposed)
         and then
         No_Wall_Between (This, Proposed, Current);
   end Acceptable;

   ----------
   -- Next --
   ----------

   function Next (This : access Puzzle;  Current : Position)
      return Positions
   is
      Next      : Positions (Possible_Moves);
      Count     : Moves := 0;
      Candidate : Position;
   begin
      for K in Offsets'Range loop
         Candidate := Current + Offsets (K);
         if Acceptable (This, Candidate, Current) then
            Count := Count + 1;
            Next (Count) := Candidate;
         end if;
      end loop;
      return Next (1 .. Count); -- may be a null range
   end Next;

   --------------
   -- Entrance --
   --------------

   function Entrance (This : access Puzzle) return Position is
   begin
      return This.Start_Point;
   end Entrance;

   --------------
   -- The_Exit --
   --------------

   function The_Exit (This : access Puzzle) return Position is
   begin
      return This.End_Point;
   end The_Exit;

   -------------
   -- At_Exit --
   -------------

   function At_Exit (This : access Puzzle;  Here : Position) return Boolean is
   begin
      return Here = This.End_Point;
   end At_Exit;

   ----------
   -- Rows --
   ----------

   function Rows (This : access Puzzle) return Natural is
   begin
      return This.Actual_Rows;
   end Rows;

   -------------
   -- Columns --
   -------------

   function Columns (This : access Puzzle) return Natural is
   begin
      return This.Actual_Columns;
   end Columns;

   ---------------------
   -- No_Wall_Between --
   ---------------------

   function No_Wall_Between
      (This : access Puzzle;  Proposed, Current : Position)
       return Boolean
   is
      This_Cell : Cell renames This.Grid (Proposed.Row, Proposed.Column);
   begin
      if Proposed.Row = Current.Row then
         if Proposed.Column > Current.Column then
            return not This_Cell.Walls (West);
         else
            return not This_Cell.Walls (East);
         end if;
      elsif Proposed.Column = Current.Column then
         if Proposed.Row > Current.Row then
            return not This_Cell.Walls (North);
         else
            return not This_Cell.Walls (South);
         end if;
      else
         raise Non_Adjacent_Cell;
      end if;
   end No_Wall_Between;

   -------------------------
   -- Delete_Wall_Between --
   -------------------------

   procedure Delete_Wall_Between
     (This : access Puzzle;  Cell1, Cell2 : Position)
   is
      This_Cell : Cell renames This.Grid (Cell1.Row, Cell1.Column);
      That_Cell : Cell renames This.Grid (Cell2.Row, Cell2.Column);
   begin
      --  either the rows will be equal or the columns will be equal
      if Cell1.Row = Cell2.Row then
         if Cell1.Column < Cell2.Column then
            This_Cell.Walls (East) := False;
            That_Cell.Walls (West) := False;
         else
            This_Cell.Walls (West) := False;
            That_Cell.Walls (East) := False;
         end if;
      elsif Cell1.Column = Cell2.Column then
         if Cell1.Row < Cell2.Row then
            This_Cell.Walls (South) := False;
            That_Cell.Walls (North) := False;
         else
            This_Cell.Walls (North) := False;
            That_Cell.Walls (South) := False;
         end if;
      else
         raise Non_Adjacent_Cell;
      end if;
   end Delete_Wall_Between;

   ------------------------
   -- North_Wall_Present --
   ------------------------

   function North_Wall_Present (This : access Puzzle;  Here : Position)
      return Boolean
   is
   begin
      return This.Grid (Here.Row, Here.Column).Walls (North);
   end North_Wall_Present;

   ------------------------
   -- South_Wall_Present --
   ------------------------

   function South_Wall_Present (This : access Puzzle;  Here : Position)
      return Boolean
   is
   begin
      return This.Grid (Here.Row, Here.Column).Walls (South);
   end South_Wall_Present;

   -----------------------
   -- East_Wall_Present --
   -----------------------

   function East_Wall_Present (This : access Puzzle;  Here : Position)
      return Boolean
   is
   begin
      return This.Grid (Here.Row, Here.Column).Walls (East);
   end East_Wall_Present;

   -----------------------
   -- West_Wall_Present --
   -----------------------

   function West_Wall_Present (This : access Puzzle;  Here : Position)
      return Boolean
   is
   begin
      return This.Grid (Here.Row, Here.Column).Walls (West);
   end West_Wall_Present;

   -------------------
   -- Make_Position --
   -------------------

   function Make_Position (Row, Column : Natural) return Position is
   begin
      return (Row => Row, Column => Column);
   end Make_Position;

   ---------
   -- Row --
   ---------

   function Row (This : Position) return Natural is
   begin
      return This.Row;
   end Row;

   ------------
   -- Column --
   ------------

   function Column (This : Position) return Natural is
   begin
      return This.Column;
   end Column;

end Maze;
