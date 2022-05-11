with Ada.Text_IO; use Ada.Text_IO;
package body Fibonacci is
   procedure Perform (Count : Positive) is

      package Io is new Ada.Text_Io.Integer_Io (Integer);
      use Io;

      Prev, Next : Integer;
      Result     : Integer;

   begin
      Prev := 0;
      Next := 1;
      for K in 1 .. Count
      loop
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
