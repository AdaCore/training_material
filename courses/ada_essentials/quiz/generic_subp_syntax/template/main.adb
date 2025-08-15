-- Which declaration(s) is (are) legal?
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

   --$ begin cut
   procedure Do_A is new Do_Something (String, String);
   -- T2 can be anything (even unconstrained), but T1 must be discrete
   --$ end cut
   --$ begin cut
   procedure Do_B is new Do_Something (Character, Character);
   -- T1 is :ada:`Character` (which is an enumerated type) - discrete
   --$ end cut
   --$ begin cut
   procedure Do_C is new Do_Something (Integer, Integer);
   -- T1 is :ada:`Integer` - discrete
   --$ end cut
   --$ begin cut
   procedure Do_D is new Do_Something (Float, Boolean);
   -- T1 is :ada:`Float` - not discrete
   --$ end cut

begin
   null;
end Main;
