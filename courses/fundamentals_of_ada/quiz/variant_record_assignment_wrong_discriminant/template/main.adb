--  Which declaration(s) is (are) legal for this piece of code?
--$ begin question
procedure Main is
   type Shape_Kind is (Circle, Line);

   type Shape (Kind : Shape_Kind) is record
      case Kind is
         when Line =>
            X, Y : Float;
            X2, Y2 : Float;
         when Circle =>
            Radius : Float;
      end case;
   end record;
   -- V and V2 declaration...
--$ end question

   --$ begin cut
   V : Shape := (Circle, others => 0.0)
   V2 : Shape (Line);
   -- Cannot assign with different discriminant
   --$ end cut

   --$ begin cut
   V : Shape := (Kind => Circle, Radius => 0.0);
   V2 : Shape (Circle);
   -- OK
   --$ end cut

   --$ begin cut
   V : Shape (Line) := (Kind => Circle, Radius => 0.0);
   V2 : Shape (Circle);
   -- ``V`` initial value has a different discriminant
   --$ end cut

   --$ begin cut
   V : Shape;
   V2 : Shape (Circle);
   -- ``Shape`` cannot be mutable: ``V`` must have a discriminant
   --$ end cut

   --$ begin question
begin
   V := V2;
   --$ end question
end Main;
