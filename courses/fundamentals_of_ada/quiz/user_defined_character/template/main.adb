-- Which of the following proposition(s) is(are) true
procedure Main is

   --$ begin question
   type T1 is (NUL, A, B, 'C');
   for T1 use (NUL => 0, A => 1, B => 2, 'C' => 3);
   type T2 is array (Positive range <>) of T1;
   Obj : T2 := "CC" & A & NUL;
   --$ end question

begin
   --$ begin cut
   -- The code fails at run-time
   This_Should_Not_Compile;
   --$ end cut
   
   pragma Assert (
      --$ line cut
      Obj'Length = 3
      --$ line cut
      Obj (1) = 'C'
      --$ line cut
      Obj (3) = A
   );
end Main;
