procedure Main is
   --$ line question
   type My_Array is array (Integer range <>) of Boolean;
   --$ line cut
   O : My_Array (2);
   --$ begin cut
   -- Multiline cut
   O : My_Array (1 .. 2);
   --$ end cut
   --$ line cut
   O : My_Array (1 .. 3);
   --$ line cut
   O : My_Array (1, 3);
   --$ begin answer
   --  You must declare the :ada:`range` using the :ada:`".."` operator
   --$ end answer
begin
   pragma Assert (O'Length = 2);
end Main;
