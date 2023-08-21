-- Which declaration(s) is(are) legal?
procedure Main is
   --$ begin question
   generic
      type T1 is (<>);
      type T2 (<>) is private;
   procedure Do_Something (A : T1; B : T2);
   --$ end question

   procedure Do_Something is
   begin
      null;
   end Do_Something;

   --$ line cut
   procedure Do_A is new Do_Something (String, String);
   --$ line cut
   procedure Do_B is new Do_Something (Character, Character);
   --$ line cut
   procedure Do_C is new Do_Something (Integer, Integer);
   --$ line cut
   procedure Do_D is new Do_Something (Boolean, Boolean);

   --$ begin answer
   -- :ada:`T2` can be almost anything, so it's not the issue
   -- :ada:`T` must be discrete, so it cannot be :ada:`String`
   --$ end answer

begin
   null;
end Main;
