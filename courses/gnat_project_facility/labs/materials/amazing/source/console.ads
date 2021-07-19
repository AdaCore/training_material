-----------------------------------------
--
--  Copyright (C) 2008-2010, AdaCore
--
-----------------------------------------

with Maze;

package Console is

   type Device (<>) is tagged limited private; --  a singleton

   type Reference is access all Device;

   type Any_Device is access all Device'Class;

   function Instance return Reference;
   --  Returns a pointer to the single instance that is maintained by
   --  this package.

   type Location is
      record
         Row, Column : Natural;
      end record;

   procedure Put
     (This  : in out Device;
      Char  : Character;
      Point : Console.Location);

   procedure Put
     (This  : in out Device;
      Str   : String;
      Point : Console.Location);

   procedure Clear (This : in out Device);

   procedure Plot_Solution_Point
     (This  : in out Device;
      Here  : Console.Location);

   procedure Plot_Intersection_Point
     (This  : in out Device;
      Here  : Console.Location);

   procedure Draw (This : in out Device;  The_Maze : access Maze.Puzzle);

   function Safe_Position
     (This : Device;  The_Maze : access Maze.Puzzle)
      return Location;
   --  a position that will not overwrite part of the maze

private

   type Device is tagged limited null record;

   The_Instance : Reference := new Device;
   --  This is the singleton pointer shared by all reference requests.

end Console;
