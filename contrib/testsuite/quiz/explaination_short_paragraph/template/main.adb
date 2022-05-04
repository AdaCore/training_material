--  How to declare an array of 2 elements?
procedure Main is
   --$ line question
   type My_Array is array (Integer range <>) of Boolean;
   --$ line cut
   O : My_Array (2);
   --$ line cut
   O : My_Array (1 .. 2);
   --$ line cut
   O : My_Array (1 .. 3);
   --$ line cut
   O : My_Array (1, 2);

   --$ begin answer
   --  You must declare the :ada:`range` using the :ada:`".."` operator.
   --  The range :ada:`1 .. 2` has a length of 2.
   --$ end answer
begin
   pragma Assert (O'Length = 2);
end Main;
