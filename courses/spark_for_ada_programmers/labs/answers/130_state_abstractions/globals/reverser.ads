with A_Stack; use A_Stack;

package Reverser with
   SPARK_Mode
is

   subtype Array_Range is Natural range 1 .. 10_000;
   type Array_Of_Items is array (Array_Range range <>) of Item;

   procedure Reverse_Array (A : in out Array_Of_Items) with
      Global => (In_Out => (Sp, Vec)),
      Pre    => A'Length > 0 and then A'Length <= Stack_Size;

end Reverser;
