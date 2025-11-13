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
      if A1 /= null then
      -- Can always check an access for :ada:`null`
      --$ end cut
        null;
      end if;
      --$ begin cut
      if A1.all'Size > 32 then
      -- :ada:`T1` is incomplete, so we don't know its size
      --$ end cut
        null;
      end if;
      --$ begin cut
      if A2 = B2 then
      -- Comparison of private types is allowed
      --$ end cut
        null;
      end if;
      --$ begin cut
      if A2 - B2 /= 0 then
      -- We do not know if :ada:`T2` allows math
      --$ end cut
        null;
      end if;

   --$ line question
   end G_P;

begin
   null;
end Main;
