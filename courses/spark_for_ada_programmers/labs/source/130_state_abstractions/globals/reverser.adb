package body Reverser with
   SPARK_Mode
is

   procedure Reverse_Array (A : in out Array_Of_Items) is
   begin
      for I in A'Range
      loop
         exit when Is_Full;
         Push (A (I));
      end loop;
      for J in A'Range
      loop
         exit when Is_Empty;
         Pop (A (J));
      end loop;
   end Reverse_Array;

end Reverser;
