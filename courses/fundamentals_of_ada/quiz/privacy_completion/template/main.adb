-- Which completion(s) is (are) correct for the type :ada:`T`?
procedure Main is
   package Pkg is
      --$ line question
      type T is private;

   private
      --$ begin cut
      type T is tagged null record;
      -- Can declare supplementary capability
      --$ end cut

      --$ begin cut
      type T is limited null record;
      -- Cannot add further constraint
      --$ end cut

      --$ begin cut
      type T is array (1 .. 10) of Integer;
      -- Note: an unconstrained `range <>` would be incorrect
      --$ end cut

      --$ begin cut
      type T is abstract tagged null record;
      -- Abstract is a constraint
      --$ end cut

   end Pkg;
begin
   null;
end Main;
