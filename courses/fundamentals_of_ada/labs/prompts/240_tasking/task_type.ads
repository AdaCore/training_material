package Task_Type is

   type Task_Id_T is new Boolean; -- pick something better!

   task type Task_T is
      entry Start_Task
        (Task_Id        : Task_Id_T;
         Delay_Duration : Duration);
      entry Stop_Task;
   end Task_T;

end Task_Type;
