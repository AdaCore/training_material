package body Fibonacci is
   procedure Perform (Count : Positive) is

      Prev, Next : Long_Long_Integer;
      Result     : Long_Long_Integer;

   begin
      Prev := 0;
      Next := 1;
      for K in Long_Long_Integer range 1 .. Long_Long_Integer (Count) loop
         Result := Prev + Next;
         Put (K);
         Put ("  ");
         Put (Result);
         New_Line;
         Prev := Next;
         Next := Result;
      end loop;
   end Perform;
end Fibonacci;
