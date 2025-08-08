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
 
      --$ begin cut
      pragma Assert (A1 /= null);
      -- Can always check a n access for Null
      --$ end cut
      --$ begin cut
      pragma Assert (A1.all'Size > 32);
      -- T1 is incomplete, so we don't know its size
      --$ end cut
      --$ begin cut
      pragma Assert (A2 = B2);
      -- Comparison of private types is allowed
      --$ end cut
      --$ begin cut
      pragma Assert (A2 - B2 /= 0);
      -- We do not know if :ada:`T2` allows math
      --$ end cut

   --$ line question
   end G_P;

begin
   null;
end Main;
