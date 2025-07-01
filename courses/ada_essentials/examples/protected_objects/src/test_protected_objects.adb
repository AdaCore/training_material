with Tasks; use Tasks;

procedure Test_Protected_Objects is
begin
   T1.Start;
   T1.Receive_Message;
   T2.Start;
   T2.Receive_Message;
   T2.Receive_Message;
   T1.Receive_Message;

   --  Ugly...
   abort T1;
   abort T2;
end Test_Protected_Objects;
