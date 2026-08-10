with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO;

package body Math with
  SPARK_Mode
is

   package IO is new Ada.Text_IO.Integer_IO (Integer);

   procedure Add
     (X, Y :     Integer;
      Z    : out Integer) is
   begin
      Z := Saturated_Add (X, Y);
   end Add;

   function Saturated_Add
     (X, Y : Integer)
      return Integer is
   begin
      if X < 0 and Y < 0 then -- both negative
         if X < Integer'First - Y then
            return Integer'First;
         else
            return X + Y;
         end if;

      elsif X > 0 and Y > 0 then -- both positive
         if X > Integer'Last - Y then
            return Integer'Last;
         else
            return X + Y;
         end if;

      else -- one positive or zero, one negative or zero, adding them is safe
         return X + Y;
      end if;
   end Saturated_Add;

   procedure Move_Along_X
     (Coord  : in out Coordinate_T;
      Change :        Integer) is
   begin
      Coord.X := Saturated_Add (Coord.X, Change);
   end Move_Along_X;

   procedure Move_Along_Y
     (Coord  : in out Coordinate_T;
      Change :        Integer) is
   begin
      Coord.Y := Saturated_Add (Coord.Y, Change);
   end Move_Along_Y;

   procedure Navigate
     (Coord    : in out Coordinate_T;
      X_Change :        Integer;
      Y_Change :        Integer) is
   begin
      Move_Along_X (Coord, X_Change);
      Move_Along_Y (Coord, Y_Change);
   end Navigate;

   procedure Convert
     (S     :     String;
      Value : out Integer) is
      Unused : Positive;
   begin
      if
        (for some C of S =>
           not
           (Is_Digit (C) or C = 'E' or C = 'e' or C = '+' or C = '-' or
            C = '_'))
      then
         raise Invalid_String;
      else
         IO.Get (S, Value, Unused);
      end if;
   exception
      when Ada.Text_IO.Data_Error =>
         raise Constraint_Error;
   end Convert;

end Math;
