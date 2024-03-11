package body Simple_Stack is

   Stack          : array (1 .. 10) of Integer;
   Next_Available : Positive := Stack'First;

   procedure Push (Item : Integer) is
   begin
      if not Full then
         Stack (Next_Available) := Item;
         Next_Available         := Next_Available + 1;
      end if;
   end Push;

   function Pop return Integer is
   begin
      if Empty then
         Next_Available := Next_Available - 1;
      end if;
      return Stack (Next_Available);
   end Pop;

   function Empty return Boolean is (Next_Available = 1);
   function Full return Boolean is (Next_Available > Stack'Last);
   function Top return Integer is (Stack (Next_Available - 1));
   function Count return Natural is (Next_Available - 1);

   procedure Reset is
   begin
      Next_Available := Stack'First;
   end Reset;

end Simple_Stack;
