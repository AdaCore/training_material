-- Which  of the following declaration(s) is (are) legal?
procedure Main is
   --$ begin question
   type T is limited record
      I : Integer;
   end record;
   --$ end question

   --$ line cut
   function "+" (A : T) return T is (A);
   --$ line cut
   function "-" (A : T) return T is (I => -A.I);
   --$ line cut
   function "=" (A, B : T) return Boolean is (True);
   --$ line cut
   function "=" (A, B : T) return Boolean is (A.I = T'(I => B.I).I);
begin
   null;
end Main;
