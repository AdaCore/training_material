package Integer_Stacks is
...
private

   type Contents is array (1 .. Max) of Integer;
   type Stack is record
      Values : Contents;
      Top    : Natural range 0 .. Max := 0;
   end record;

end Integer_Stacks;
