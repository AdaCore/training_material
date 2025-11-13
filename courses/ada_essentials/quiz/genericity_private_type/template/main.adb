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
      Flag : Boolean;
   begin
      -- Complete here
   --$ end question
 
      --$ begin cut
      Flag := A1 /= null;
      -- Can always check a n access for :ada:`null`
      --$ end cut
      --$ begin cut
      Flag := A1.all'Size > 32;
      -- :ada:`T1` is incomplete, so we don't know its size
      --$ end cut
      --$ begin cut
      Flag := A2 = B2;
      -- Comparison of private types is allowed
      --$ end cut
      --$ begin cut
      Flag := A2 - B2 /= 0;
      -- We do not know if :ada:`T2` allows math
      --$ end cut

   --$ line question
   end G_P;

begin
   null;
end Main;
