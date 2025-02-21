-- Which proposition(s) will compile and run without error?
procedure Main is
   --$ begin question
   type R (Size : Integer := 0) is record
      S : String (1 .. Size);
   end record;
   --$ end question

   --$ begin cut
   V : R := (6, "Hello");
   --$ end cut
   --$ line cut
   V : R := (5, "Hello");
   --$ begin cut
   V : R (5) := (5, S => "Hello");
   --$ end cut
   --$ begin cut
   V : R (6) := (6, S => "Hello");
   --$ end cut

   --$ begin answer
   -- Choices **A** and **B** are mutable: the runtime assumes :ada:`Size`
   -- can be :ada:`Positive'Last`, so component :ada:`S` will cause a run-time error.
   -- Choice **D** tries to copy a 5-character string into a 6-character string,
   -- also generating a run-time error.
   --$ end answer
begin
   null;
end Main;
