with Protected_Object;
with Task_Type;
procedure Main is

   T1, T2           : Task_Type.Task_T;
   Last_Id, This_Id : Task_Type.Task_Id_T := Task_Type.Task_Id_T'last;

   use type Task_Type.Task_Id_T;

begin

   T1.Start_Task (Task_Type.Task_Id_T'last, 0.3);
   T2.Start_Task (Task_Type.Task_Id_T'first, 0.5);

   -- Some kind of loop with a delay

   T1.Stop_Task;
   T2.Stop_Task;

end Main;
