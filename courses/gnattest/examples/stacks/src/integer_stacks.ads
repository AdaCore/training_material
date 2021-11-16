--$ begin cut
package Integer_Stacks is
--$ end cut
   type Stack is limited private;
   Max : constant := 100;
 
   procedure Push (This  : in out Stack;
                   Input : in Integer);
   procedure Pop (This    : in out Stack;
                   Output : out Integer);
--$ begin cut
private

   type Contents is array (1 .. Max) of Integer;
   type Stack is record
      Values : Contents;
      Top    : Natural range 0 .. Max := 0;
   end record;

end Integer_Stacks;
--$ end cut
