package Math with
  SPARK_Mode
is

   Invalid_String : exception;

   function Saturated_Add
     (X, Y : Integer)
      return Integer;

   procedure Add
     (X, Y :     Integer;
      Z    : out Integer) with
     Post => Z = Integer'First or Z = Integer'Last or Z = X + Y;

   procedure Convert
     (S     :     String;
      Value : out Integer);

end Math;
