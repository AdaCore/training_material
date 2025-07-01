-- Which of the following statement(s) is (are) legal for ``G_P``'s body?
procedure Main is
   --$ begin question
   generic
      type T1;
      A1 : access T1;
      type T2 is private;
      A2, B2 : T2;
   procedure G_P;
   procedure G_P is
   begin
      -- Complete here
   --$ end question
 
      --$ line cut
      pragma Assert (A1 /= null);
      --$ line cut
      pragma Assert (A1.all'Size > 32);
      --$ line cut
      pragma Assert (A2 = B2);
      --$ line cut
      pragma Assert (A2 - B2 /= 0);

   --$ line question
   end G_P;

begin
   null;
end Main;
