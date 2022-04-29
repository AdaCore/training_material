-- Which proposition(s) will compile and run without error?
procedure Main is
   --$ begin question
   type R (Size : Integer := 0) is record
      S : String (1 .. Size);
   end record;
   --$ end question
   --$ begin cut
   V1 : R := (6, "Hello");
   --$ end cut
   --$ line cut
   V1 : R := (5, "Hello");
   --$ begin cut
   V1 : R (5) := (5, S => "Hello");
   --$ end cut
   --$ begin cut
   V1 : R (6) := (6, S => "Hello");
   --$ end cut
begin
   null;
end Main;
