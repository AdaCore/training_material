-- Which of the following completions are legal for :ada:`type T`? (Select all that apply)
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
