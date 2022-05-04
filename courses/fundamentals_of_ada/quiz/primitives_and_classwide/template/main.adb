-- Which of the following piece(s) of code allow for calling :ada:`Proc (V)`?

procedure Main is
   --$ begin question
   type T1 is tagged null record;
   type T2 is new T1 with null record;
   V : T2;
   --$ end question

   --$ begin cut
   procedure Proc (V : T1) is null;
   --  Proc is not a primitive
   --$ end cut

   --$ begin cut
   procedure Proc (V : T1'Class) is null;
   --$ end cut

   --$ begin cut
   procedure Proc (V : T1'Class) is null;
   procedure Proc (V : T2'Class) is null;
   -- :ada:`T1'Class` contains :ada:`T2'Class`
   --$ end cut

   --$ begin cut
   procedure Proc (V : T1) is null;
   procedure Proc (V : T2) is null;
   --$ end cut

begin
   Proc (V);
end Main;
