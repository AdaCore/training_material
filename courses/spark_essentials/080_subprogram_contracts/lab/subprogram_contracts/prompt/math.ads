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
      return Integer;
   --  Return X + Y, capping at Integer'Last or
   --  Integer'First on overflow

   procedure Move_Along_X
     (Coord  : in out Coordinate_T;
      Change :        Integer);
   --  Update the X field by Change

   procedure Move_Along_Y
     (Coord  : in out Coordinate_T;
      Change :        Integer);
   --  Update the Y field by Change

   procedure Navigate
     (Coord    : in out Coordinate_T;
      X_Change :        Integer;
      Y_Change :        Integer);
   --  Update Coord appropriately

   procedure Convert
     (S     :     String;
      Value : out Integer);
   --  Convert S to an Integer
   --  (Exception will be raised for invalid data)

end Math;
