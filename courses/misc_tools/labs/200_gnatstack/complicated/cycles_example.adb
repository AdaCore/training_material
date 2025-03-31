package body Cycles_Example is

   procedure Odd (Number : in out Integer) is
   begin
      Number := Number - 1;
      if Number > 0
      then
         Cycles (Number);
      end if;
   end Odd;

   procedure Even (Number : in out Integer) is
   begin
      Number := Number - 2;
      if Number > 0
      then
         Cycles (Number);
      end if;
   end Even;

   procedure Cycles (Number : in out Integer) is
      Half : constant Integer := Number / 2;
   begin
      if Half * 2 = Number
      then
         Even (Number);
      else
         Odd (Number);
      end if;
   end Cycles;

end Cycles_Example;
