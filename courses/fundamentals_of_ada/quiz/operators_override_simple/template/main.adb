-- Which operator(s) definition(s) is legal?
procedure Main is
   --$ line question
   type T is new Integer;

   --$ line cut
   function "+" (V : T) return Boolean is (T /= 0);
   --$ begin cut
   function "+" (A, B : T) return T is (A + B);
   -- Infinite recursion
   --$ end cut
   --$ line cut
   function "=" (A, B : T) return T is (A - B);
   --$ begin cut
   function ":=" (A : T) return T is (A);
   -- Unlike some languages, there is no assignment operator
   --$ end cut
begin
   null;
end Main;
