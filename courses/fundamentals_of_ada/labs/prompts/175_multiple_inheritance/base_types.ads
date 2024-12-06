package Base_Types is
   type Coordinate_T is record
      X_Coord : Integer;
      Y_Coord : Integer;
   end record;

   type Line_T is array (1 .. 2) of Coordinate_T;
   type Lines_T is array (Natural range <>) of Line_T;

end Base_Types;
