package Task_Type is
   type Task_Id_T is range 1_000 .. 9_999;
   task type Task_T is
      entry Start_Task (Task_Id : Task_Id_T; Delay_Duration : Duration);
      entry Stop_Task;
   end Task_T;
end Task_Type;
