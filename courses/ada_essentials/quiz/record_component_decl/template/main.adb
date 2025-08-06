-- Which record definition(s) is (are) legal?

procedure Main is
   --$ begin question
   type Record_T is record
      -- Definition here
      --$ begin cut
      Component_1 : array (1 .. 3) of Boolean;
      -- Anonymous types not allowed
      --$ end cut
      --$ begin cut
      Component_2, Component_3 : Integer;
      -- Correct
      --$ end cut
      --$ begin cut
      Component_1 : Record_T;
      -- No recursive definition
      --$ end cut
      --$ begin cut
      Component_1 : constant Integer := 123;
      -- No constant component
      --$ end cut
   --$ end question
   --$ line question
   end record;
begin
   null;
end Main;
