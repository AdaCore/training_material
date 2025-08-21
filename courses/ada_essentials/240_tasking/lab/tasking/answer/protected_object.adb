package body Protected_Object is
   protected body Monitor is
      procedure Set (Id : Task_Type.Task_Id_T) is
      begin
         Value := Id;
      end Set;
      function Get return Task_Type.Task_Id_T is (Value);
   end Monitor;
end Protected_Object;
