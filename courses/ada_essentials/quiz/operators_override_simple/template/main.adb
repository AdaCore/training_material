-- Which of the following operator definitions are legal? (Select all that apply)
procedure Main is
   --$ line question
   type T is new Integer;

   --$ begin cut
   function "+" (V : T) return Boolean is (V /= 0);
   -- Legal
   --$ end cut
   --$ begin cut
   function "+" (A, B : T) return T is (A + B);
   -- Infinite recursion (will result in Storage_Error at run-time)
   --$ end cut
   --$ begin cut
   function "=" (A, B : T) return T is (A - B);
   -- Legal
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
