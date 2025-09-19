package body Employee is
   function Create_Staff (First_Name  : String;
                          Last_Name   : String;
                          Hourly_Rate : Pay_T)
                          return Employee_T is
      Ret_Val : Employee_T;
   begin
      Ret_Val := (Staff,
                  To_Vstring (Last_Name),
                  To_Vstring (First_Name),
                  Hourly_Rate);
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
      Ret_Val := (Category    => Supervisor,
                  Last_Name   => To_Vstring (Last_Name),
                  First_Name  => To_Vstring (First_Name),
                  Hourly_Rate => Hourly_Rate,
                  Project     => To_Vstring (Project));
      return Ret_Val;
   end Create_Supervisor;

   function Create_Manager (First_Name  : String;
                            Last_Name   : String;
                            Hourly_Rate : Pay_T;
                            Department  : String;
                            Staff_Count : Natural)
                            return Employee_T is
      Ret_Val : Employee_T (Manager);
   begin
      Ret_Val.Last_Name   := To_Vstring (Last_Name);
      Ret_Val.First_Name  := To_Vstring (First_Name);
      Ret_Val.Hourly_Rate := Hourly_Rate;
      Ret_Val.Department  := To_Vstring (Department);
      Ret_Val.Staff_Count := Staff_Count;
      return Ret_Val;
   end Create_Manager;
end Employee;
