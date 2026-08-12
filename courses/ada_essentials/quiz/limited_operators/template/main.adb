-- Which of the following declarations are legal? (Select all that apply)
procedure Main is
   --$ begin question
   type T is limited record
      I : Integer;
   end record;
   --$ end question

   --$ begin cut
   function "+" (A : T) return T is (A);
   -- Returning a copy of a limited object is not allowed
   --$ end cut
   --$ begin cut
   function "-" (A : T) return T is (I => -A.I);
   -- Creating a new object
   --$ end cut
   --$ begin cut
   function "=" (A, B : T) return Boolean is (True);
   -- No actual comparison happening
   --$ end cut
   --$ begin cut
   function "=" (A, B : T) return Boolean is (A.I = T'(I => B.I).I);
   -- Comparing components is allowed
   --$ end cut
begin
   null;
end Main;
