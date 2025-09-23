package body Employee is
   function Create_Staff (First_Name  : String;
                          Last_Name   : String;
                          Hourly_Rate : Pay_T)
                          return Employee_T is
      Ret_Val : Employee_T;
   begin
      return Ret_Val;
   end Create_Staff;

   function Create_Supervisor
     (First_Name  : String;
      Last_Name   : String;
      Hourly_Rate : Pay_T;
      Project     : String)
      return Employee_T is
      Ret_Val : Employee_T;
   begin
      return Ret_Val;
   end Create_Supervisor;

   function Create_Manager (First_Name  : String;
                            Last_Name   : String;
                            Hourly_Rate : Pay_T;
                            Department  : String;
                            Staff_Count : Natural)
                            return Employee_T is
      Ret_Val : Employee_T;
   begin
      return Ret_Val;
   end Create_Manager;
end Employee;
