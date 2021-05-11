
with Ada.Text_IO; use Ada.Text_IO;

with Messages;
with Messages.Queue;
with Messages.Queue.Debug;

procedure Main is
   Char    : Character := 'A';
   Content : String (1 .. 10);
   Message : Messages.Message_T;
   Valid   : Boolean;
begin
   while not Messages.Queue.Full
   loop
      Content := (others => Char);
      Messages.Queue.Push (Messages.Create
           (Kind    => Messages.Command,
            Content => Content));
      Char := Character'Succ (Char);
   end loop;

   Put_Line
     ("Added " & Integer'Image (Messages.Queue.Debug.Queue_Length) &
      " messages");

   -- inject some faults
   Messages.Queue.Debug.Inject_Crc_Fault (3);
   Messages.Queue.Debug.Inject_Crc_Fault (6);

   while not Messages.Queue.Empty
   loop
      Put (Integer'Image (Messages.Queue.Debug.Queue_Length) & ") ");
      Messages.Queue.Pop (Message, Valid);
      Put_Line
        (Boolean'Image (Valid) & " " & Messages.Queue.Debug.Text (Message));
   end loop;

end Main;
