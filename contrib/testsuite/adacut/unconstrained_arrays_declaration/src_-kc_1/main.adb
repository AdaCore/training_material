procedure Main is
   type My_Array is array (Integer range <>) of Boolean;
   O : My_Array (2);
begin
   pragma Assert (O'Length = 2);
end Main;
