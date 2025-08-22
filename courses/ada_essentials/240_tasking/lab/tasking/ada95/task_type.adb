with Protected_Object;
package body Task_Type is
   task body Task_T is
      Wait_Time : Duration;
      Id        : Task_Id_T;
   begin
      accept Start_Task (Task_Id        : Task_Id_T;
                         Delay_Duration : Duration) do
         Wait_Time := Delay_Duration;
         Id        := Task_Id;
      end Start_Task;
      loop
         select
            accept Stop_Task;
            exit;
         or
            delay Wait_Time;
            Protected_Object.Monitor.Set (Id);
         end select;
      end loop;
   end Task_T;
end Task_Type;
