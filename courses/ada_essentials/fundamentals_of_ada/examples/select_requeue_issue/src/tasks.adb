package body Tasks is

   --$ begin cut
   task body Select_Requeue_Quit is
   begin
      accept Receive_Message (V : String) do
         requeue Receive_Message;
      end Receive_Message;
      delay 2.0;
   end Select_Requeue_Quit;
   --$ end cut

   task body Call_Or_Terminate is
   begin
      --$ begin cut
      select
         Select_Requeue_Quit.Receive_Message ("Hello");
      or
         delay 0.1;
      end select;
      --$ end cut
   end Call_Or_Terminate;
end Tasks;
