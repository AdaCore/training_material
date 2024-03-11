package Simple_Stack is

   procedure Push (Item : Integer);
   function Pop return Integer;
   function Empty return Boolean;
   function Full return Boolean;
   function Top return Integer;
   function Count return Natural;
   procedure Reset;

end Simple_Stack;
