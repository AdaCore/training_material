-- Which of the following completion(s) of :ada:`T` is(are) valid?
procedure Main is
   --$ begin question
   package Pkg is
      type T is private;
   private
      --$ end question

      --$ line cut
      type T;
      --$ line cut
      type T is tagged null record;
      --$ line cut
      type T is limited null record;
      --$ line cut
      type T is new Integer;
   end Pkg;

begin
   null;
end Main;
