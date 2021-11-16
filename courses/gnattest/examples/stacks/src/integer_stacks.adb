package body Integer_Stacks is
   procedure Push (This  : in out Stack;
                   Input : in Integer) is
   begin
      This.Top := This.Top + 1;
      This.Values (This.Top) := Input;
   end Push;
 
   procedure Pop (This    : in out Stack;
                   Output : out Integer) is
   begin
      Output := This.Values (This.Top);
      This.Top := This.Top - 1;
   end Pop;
end Integer_Stacks;
