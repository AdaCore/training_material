package Math with
  SPARK_Mode
is

   Invalid_String : exception;

   type Coordinate_T is record
      X : Integer;
      Y : Integer;
   end record;

   procedure Add
     (X, Y :     Integer;
      Z    : out Integer) with
     Post => Z in Integer'First | Integer'Last | X + Y;
   --  Z will be the result of a saturated
   --  addition of X and Y

   function Saturated_Add
     (X, Y : Integer)
      return Integer with
     Contract_Cases =>
      ((X + Y in Integer)    => Saturated_Add'Result = X + Y,
       X + Y < Integer'First => Saturated_Add'Result = Integer'First,
       X + Y > Integer'Last  => Saturated_Add'Result = Integer'Last);

   --  Return X + Y, capping at Integer'Last or
   --  Integer'First on overflow

   procedure Move_Along_X
     (Coord  : in out Coordinate_T;
      Change :        Integer) with
     Post =>
      Coord.X = Saturated_Add (Coord.X'Old, Change) and Coord.Y = Coord.Y'Old;
   --  Update the X field by Change

   procedure Move_Along_Y
     (Coord  : in out Coordinate_T;
      Change :        Integer) with
     Post => Coord = (Coord.X'Old, Saturated_Add (Coord.Y'Old, Change));
   --  Update the Y field by Change

   procedure Navigate
     (Coord    : in out Coordinate_T;
      X_Change :        Integer;
      Y_Change :        Integer) with
     Post =>
      Coord =
      (Saturated_Add (Coord.X'Old, X_Change),
       Saturated_Add (Coord.Y'Old, Y_Change));
   --  Update Coord appropriately

   procedure Convert
     (S     :     String;
      Value : out Integer) with
     Exceptional_Cases => (Invalid_String | Constraint_Error => True);
   --  Convert S to an Integer
   --  (Exception will be raised for invalid data)

end Math;
