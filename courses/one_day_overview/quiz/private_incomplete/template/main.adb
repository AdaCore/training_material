-- Which of the following declaration(s) is(are) valid?
procedure Main is
   --$ begin question
   package Pkg is
      type T is private;
   private
      -- Declarations Here
      --$ end question

      --$ begin cut
      type T is array (Positive range <>) of Integer;
      -- Cannot complete with an unconstrained type
      --$ end cut

      --$ begin cut
      type T is tagged null record;
      -- Can complete with the :ada:`tagged` capability
      --$ end cut

      --$ begin cut
      type T is limited null record;
      -- Cannot complete with a :ada:`limited` constraint
      --$ end cut

      --$ begin cut
      type T_Arr is array (Positive range <>) of T;
      type T is new Integer;
      -- Even though T is private, it can be used as component
      --$ end cut
   end Pkg;

begin
   null;
end Main;
