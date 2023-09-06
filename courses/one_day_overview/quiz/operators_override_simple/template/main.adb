-- Which operator(s) definition(s) is legal?
procedure Main is
   --$ line question
   type T is new Integer;

   --$ line cut
   function "+" (V : T) return Boolean is (V /= 0);
   --$ begin cut
   function "+" (A, B : T) return T is (A + B);
   -- Infinite recursion
   --$ end cut
   --$ begin cut
   function "=" (A, B : T) return T is (A - B);
   --$ end cut
   --$ begin cut
   function ":=" (A : T) return T is (A);
   -- Unlike some languages, there is no assignment operator
   --$ end cut

   A, B, C : T := 1;
begin
   C := A + B;
   pragma Assert (C = 2);
end Main;
