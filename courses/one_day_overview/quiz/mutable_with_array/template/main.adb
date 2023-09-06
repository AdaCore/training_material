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
   -- When V is declared without specifying its size, it becomes mutable,
   -- at this point the :ada:`S'Length = Positive'Last`, causing a Runtime_Error.
   -- Furthermore the length of "Hello" is 5, it cannot be stored in a String of Length 6.
   --$ end answer
begin
   null;
end Main;
