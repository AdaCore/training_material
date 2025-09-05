with Ada.Text_IO; use Ada.Text_IO;
with Protected_Object;
with Task_Type;

procedure Main is
   T1, T2, T3       : Task_Type.Task_T;
   Last_Id, This_Id : Task_Type.Task_Id_T := Task_Type.Task_Id_T'Last;
   use type Task_Type.Task_Id_T;
begin

   T1.Start_Task (1_111, 0.3);
   T2.Start_Task (2_222, 0.5);
   T3.Start_Task (3_333, 0.7);

   for Count in 1 .. 20 loop
      This_Id := Protected_Object.Monitor.Get;
      if Last_Id /= This_Id then
         Last_Id := This_Id;
         Put_Line (Count'Image & "> " & Last_Id'image);
      end if;
      delay 0.2;
   end loop;

   T1.Stop_Task;
   T2.Stop_Task;
   T3.Stop_Task;

end Main;
