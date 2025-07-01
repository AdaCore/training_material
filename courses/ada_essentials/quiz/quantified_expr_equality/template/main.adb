-- Which piece(s) of code correctly perform(s) equality check on A and B?
procedure Main is
   --$ begin question
   type T1 is array (1 .. 3) of Integer;
   type T2 is array (1 .. 3) of Integer;
   --$ end question

   --$ begin cut
   function "=" (A : T1; B : T2) return Boolean is
     (A = T1 (B));
   --$ end cut
   --$ begin cut
   function "=" (A : T1; B : T2) return Boolean is
     (for all E1 of A => (for all E2 of B => E1 = E2));
   -- Counterexample: ``A = B = (0, 1, 0)`` returns :ada:`False`
   --$ end cut
   --$ begin cut
   function "=" (A : T1; B : T2) return Boolean is
     (for some E1 of A => (for some E2 of B => E1 = E2));
   -- Counterexample: ``A = (0, 0, 1) and B = (0, 1, 1)`` returns :ada:`True`
   --$ end cut
   --$ begin cut
   function "=" (A : T1; B : T2) return Boolean is
     (for all J in A'Range => A (J) = B (J));
   --$ end cut

   type Answer_T is record
      Equal : Boolean;
      A     : T1;
      B     : T2;
   end record;

   Answers : array (Positive range <>) of Answer_T :=
     ((True, (0, 0, 1), (0, 0, 1)), (False, (0, 0, 1), (0, 1, 1)));
begin
   pragma Assert (for all Ans of Answers => Ans.Equal = (Ans.A = Ans.B));
end Main;
