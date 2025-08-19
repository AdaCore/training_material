with Protected_Object;
package body Task_Type is

   task body Task_T is
   begin
      accept Start_Task
        (Task_Id        : Task_Id_T;
         Delay_Duration : Duration);
      accept Stop_Task;
      delay 0.0;
   end Task_T;

end Task_Type;
