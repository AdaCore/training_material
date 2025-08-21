--Types
package Base_Types is

   type Coordinate_T is record
      X_Coord : Integer;
      Y_Coord : Integer;
   end record;
   function Image (Coord : Coordinate_T) return String is
     ("(" & Coord.X_Coord'Image & "," &
            Coord.Y_Coord'Image & " )");

   type Line_T is array (1 .. 2) of Coordinate_T;
   type Lines_T is array (Natural range <>) of Line_T;

   type Color_T is mod 256;
   type Width_T is range 1 .. 10;

end Base_Types;
