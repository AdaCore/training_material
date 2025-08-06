task body Select_Requeue_Quit is
begin
   accept Receive_Message (V : String) do
      requeue Receive_Message;
   end Receive_Message;
   delay 2.0;
end Select_Requeue_Quit;
   ...
   select
      Select_Requeue_Quit.Receive_Message ("Hello");
   or
      delay 0.1;
   end select;
