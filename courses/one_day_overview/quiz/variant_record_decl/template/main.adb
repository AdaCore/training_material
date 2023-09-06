--  Which declaration(s) is(are) legal?
procedure Main is
   --$ begin question
   type Shape_Kind is (Circle, Line);

   type Shape (Kind : Shape_Kind) is record
      case Kind is
         when Line =>
            X, Y : Float;
            X2, Y2 : Float;
   --$ end question

         --$ begin cut
         when Circle =>
            Cord : Shape (Line);
            -- Referencing itself
         --$ end cut

         --$ begin cut
         when Circle =>
            Center : array (1 .. 2) of Float;
            Radius : Float;
            -- anonymous array in record declaration
         --$ end cut

         --$ begin cut
         when Circle =>
            Center_X, Center_Y : Float;
            Radius : Float;
            -- OK
         --$ end cut

         --$ begin cut
         when Circle =>
            X, Y, Radius : Float;
            -- X, Y are duplicated with the Line variant
         --$ end cut

      end case;
   end record;
begin
   null;
end Main;
