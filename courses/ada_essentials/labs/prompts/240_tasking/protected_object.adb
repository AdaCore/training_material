package body Protected_Object is

   protected body Monitor is
      procedure Set (Id : Task_Type.Task_Id_T) is null;
      function Get return Task_Type.Task_Id_T is (Task_Type.Task_Id_T'Last);
   end Monitor;

end Protected_Object;
