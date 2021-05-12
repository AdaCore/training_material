-----------------------------------------
--
--  Copyright (C) 2008-2010, AdaCore
--
-----------------------------------------

with ANSI_Terminal;
with GNAT.IO;  use GNAT.IO;
with System;

package body Console is

   procedure Flush_Output;
   --  flushes stdout

   --------------
   -- Instance --
   --------------

   function Instance return Reference is
   begin
      return The_Instance;
   end Instance;

   --------------
   -- ANSI_CRT --
   --------------

   package ANSI_CRT is new ANSI_Terminal (Put);

   ---------
   -- Put --
   ---------

   procedure Put
     (This  : in out Device;
      Char  : Character;
      Point : Console.Location)
   is
      pragma Unreferenced (This);
   begin
      ANSI_CRT.Move_Cursor (To => (Point.Row, Point.Column));
      Put (Char);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (This  : in out Device;
      Str   : String;
      Point : Console.Location)
   is
      pragma Unreferenced (This);
   begin
      ANSI_CRT.Move_Cursor (To => (Point.Row, Point.Column));
      Put (Str);
   end Put;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Device) is
      pragma Unreferenced (This);
   begin
      ANSI_CRT.Clear_Screen;
   end Clear;

   -------------------------
   -- Plot_Solution_Point --
   -------------------------

   procedure Plot_Solution_Point
     (This  : in out Device;
      Here  : Console.Location)
   is
      Physical_Address : Console.Location;
      Rectangle_Fill   : constant String := " # ";
   begin
      Physical_Address.Row := Here.Row * 2;
      Physical_Address.Column := Here.Column * 4 - 2;
      Put (This, Rectangle_Fill, Physical_Address);
   end Plot_Solution_Point;

   -----------------------------
   -- Plot_Intersection_Point --
   -----------------------------

   procedure Plot_Intersection_Point
     (This  : in out Device;
      Here  : Console.Location)
   renames Plot_Solution_Point;

   --  these are the strings used to depict individual maze cells, which is
   --  why some contain spaces
   Horizontal_Wall       : constant String := "+---"; --  both North and South
   Empty_Horizontal_Wall : constant String := "+   ";
   West_Wall             : constant String := "|   ";
   Empty_West_Wall       : constant String := "    ";
   East_Wall             : constant String := "|";
   Corner                : constant String := "+";

   ----------
   -- Draw --
   ----------

   procedure Draw (This : in out Device;  The_Maze : access Maze.Puzzle) is
      R_Offset : Integer;
      C_Offset : Integer;
      Current  : Maze.Position;
      use Maze;
   begin
      --  The row and column offsets translate between the logical row/column
      --  values and the physical screen coordinates of the characters used to
      --  represent the cells. This is necessary because each cell is depicted
      --  by several characters both horizontally and vertically, such that the
      --  screen coordinates for a given maze cell are not simply at their
      --  corresponding maze row/column pair.

      R_Offset := 0;

      for R in 1 .. Maze.Rows (The_Maze) loop

         C_Offset := 0;

         for C in 1 .. Maze.Columns (The_Maze) loop

            Current := Make_Position (R, C);

            if North_Wall_Present (The_Maze, Current) then
               Put (This,
                    Horizontal_Wall,
                    Location'(R + R_Offset, C + C_Offset));
            else
               Put (This,
                    Empty_Horizontal_Wall,
                    Location'(R + R_Offset, C + C_Offset));
            end if;
            if West_Wall_Present (The_Maze, Current) then
               Put (This,
                    West_Wall,
                    Location'(R + R_Offset + 1, C + C_Offset));
            else
               Put (This,
                    Empty_West_Wall,
                    Location'(R + R_Offset + 1, C + C_Offset));
            end if;

            if R = Maze.Rows (The_Maze) then
               --  last row, so print the bottom border
               if South_Wall_Present (The_Maze, Current) then
                  Put (This,
                       Horizontal_Wall,
                       Location'(R + R_Offset + 2, C + C_Offset));
               else
                  Put (This,
                       Empty_Horizontal_Wall,
                       Location'(R + R_Offset + 2, C + C_Offset));
               end if;
               if C = Maze.Columns (The_Maze) then -- last row and column
                  --  print the bottom-right corner
                  Put (This,
                       Corner,
                       Location'(R + R_Offset + 2, C + C_Offset + 4));
               end if;
            end if;

            C_Offset := C_Offset + 3;
            --  so that we are now on the east edge

            --  print the right-most corners on the east border and each east
            --  wall that is present
            if C = Maze.Columns (The_Maze) then -- last column
               Put (This, Corner, Location'(R + R_Offset, C + C_Offset + 1));
               if East_Wall_Present (The_Maze, Current) then
                  Put (This,
                       East_Wall,
                       Location'(R + R_Offset + 1, C + C_Offset + 1));
               end if;
            end if;
         end loop; --  columns

         R_Offset := R_Offset + 1;
      end loop; --  rows

      Flush_Output;
   end Draw;

   -------------------
   -- Safe_Position --
   -------------------

   function Safe_Position
     (This : Device;  The_Maze : access Maze.Puzzle)
      return Console.Location
   is
      pragma Unreferenced (This);
   begin
      return Console.Location'((Maze.Rows (The_Maze) * 2) + 4, 1);
   end Safe_Position;

   ------------------
   -- Flush_Output --
   ------------------

   procedure Flush_Output is
      procedure Fflush (Strptr : System.Address);
      pragma Import (C, Fflush, "fflush");

      function Stdout return System.Address;
      pragma Import (C, Stdout, "__gnat_constant_stdout");
   begin
      Fflush (Stdout);
   end Flush_Output;

end Console;
