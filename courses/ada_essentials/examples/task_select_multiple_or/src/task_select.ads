package Task_Select is

   Termination_Flag : Boolean := False;

   task Select_Loop_Task is
      entry Receive_Message (V : String);
      entry Send_Message (V : String);
      entry Stop;
   end Select_Loop_Task;

end Task_Select;
