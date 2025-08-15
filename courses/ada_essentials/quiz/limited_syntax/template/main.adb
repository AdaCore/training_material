-- Which statement(s) is (are) legal?

procedure Main is
   --$ begin question
   type T is limited record
      I : Integer;
   end record;

   L1, L2 : T;
   B : Boolean;
   --$ end question
begin
   --$ begin cut
   L1.I := 1;
   -- Element is not limited
   --$ end cut

   --$ begin cut
   L1 := L2;
   -- No copy of limited objects
   --$ end cut

   --$ begin cut
   B := (L1 = L2);
   -- No comparison of limited objects
   --$ end cut

   --$ begin cut
   B := (L1.I = L2.I);
   -- Elements can be compared
   --$ end cut
end Main;
