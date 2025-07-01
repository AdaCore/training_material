package Tasks is

   task Select_Requeue_Quit is
      entry Receive_Message (V : String);
   end Select_Requeue_Quit;

   task Call_Or_Terminate;

end Tasks;
