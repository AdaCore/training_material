package Base_Types is

   type Coordinate_T is record
      X_Coord : Integer;
      Y_Coord : Integer;
   end record;

   type Line_T is array (1 .. 2) of Coordinate_T;
   -- convert Line_T so lowest X value is first
   function Ordered
     (Line : Line_T)
      return Line_T;
   type Lines_T is array (Natural range <>) of Line_T;

   type Color_Range_T is mod 255;
   type Color_T is record
      Red   : Color_Range_T;
      Green : Color_Range_T;
      Blue  : Color_Range_T;
   end record;

private
   function Ordered
     (Line : Line_T)
      return Line_T is
     (if Line (1).X_Coord > Line (2).X_Coord then (Line (2), Line (1))
      else Line);

end Base_Types;
