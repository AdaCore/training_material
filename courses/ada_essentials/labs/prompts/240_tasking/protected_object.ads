with Task_Type;
package Protected_Object is

   protected Monitor is
      procedure Set (Id : Task_Type.Task_Id_T);
      function Get return Task_Type.Task_Id_T;
   private
      Value : Task_Type.Task_Id_T;
   end Monitor;

end Protected_Object;
