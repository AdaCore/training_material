--  How to declare an array of two elements?
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
   O : My_Array (1, 3);
begin
   pragma Assert (O'Length = 2);
end Main;
