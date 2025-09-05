package body Employee_Data is

   Last_Used_Id : Id_T := Id_T'First;

   function Create
     (Name : Name_T; Rate : Hourly_Rate_T := 0.0) return Employee_T is
   begin
      return Ret_Val : Employee_T do
         Last_Used_Id := Id_T'Succ (Last_Used_Id);
         Ret_Val.Name := Name;
         Ret_Val.Rate := Rate;
         Ret_Val.Id := Last_Used_Id;
      end return;
   end Create;

   function Id (Employee : Employee_T) return Id_T
   is (Employee.Id);
   function Name (Employee : Employee_T) return Name_T
   is (Employee.Name);
   function Rate (Employee : Employee_T) return Hourly_Rate_T
   is (Employee.Rate);

end Employee_Data;
