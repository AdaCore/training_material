-- Which declaration(s) of ``F`` is(are) valid?
procedure Main is
   --$ begin question
   type T is limited record
      I : Integer;
   end record;
   --$ end question
   
   --$ line cut
   function F return T is (I := 1);
   --$ line cut
   function F return T is (I => 1);
   --$ line cut
   function F return T is (1);
   --$ begin cut
   function F return T is
   begin
      return R : T do
         R.I := 1;
      end return;
   end F;
   --$ end cut

   --$ line question
   O : T := F;

begin
   null;
end Main;
