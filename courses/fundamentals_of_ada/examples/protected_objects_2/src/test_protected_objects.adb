with Tasks;             use Tasks;
with Protected_Objects; use Protected_Objects;

procedure Test_Protected_Objects is
begin
   O1.Initialize ('X');
   O2.Initialize ('Y');
   T1.Start ('A', 1, 2);
   T2.Start ('B', 1_000, 2_000);
   T1.Receive_Message (1, 2);
   T2.Receive_Message (10, 20);

   --  Ugly...
   abort T1;
   abort T2;
end Test_Protected_Objects;
