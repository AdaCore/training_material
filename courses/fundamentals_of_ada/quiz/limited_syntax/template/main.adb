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
   --$ line cut
   L1.I := 1;

   --$ line cut
   L1 := L2;

   --$ line cut
   B := (L1 = L2);

   --$ line cut
   B := (L1.I = L2.I);
end Main;
