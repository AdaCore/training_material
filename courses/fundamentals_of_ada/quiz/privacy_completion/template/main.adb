-- Which completion(s) is(are) correct for the type :ada:`T`?
procedure Main is
   package Pkg is
      --$ line question
      type T is private;

   private
      --$ line cut
      type T is tagged null record;
      --$ line cut
      type T is limited null record;
      --$ line cut
      type T is array (1 .. 10) of Integer;
      --$ line cut
      type T is abstract tagged null record;
   end Pkg;
begin
   null;
end Main;
