package Math with
  SPARK_Mode
is

   Invalid_String : exception;

   function Saturated_Add
     (X, Y : Integer)
      return Integer with
     SPARK_Mode,
     Contract_Cases =>
      ((X + Y in Integer)    => Saturated_Add'Result = X + Y,
       X + Y < Integer'First => Saturated_Add'Result = Integer'First,
       X + Y > Integer'Last  => Saturated_Add'Result = Integer'Last);

   procedure Add
     (X, Y :     Integer;
      Z    : out Integer) with
     Post => Z = Integer'First or Z = Integer'Last or Z = X + Y;

   procedure Convert
     (S     :     String;
      Value : out Integer) with
     Exceptional_Cases => (Invalid_String | Constraint_Error => True);

end Math;
