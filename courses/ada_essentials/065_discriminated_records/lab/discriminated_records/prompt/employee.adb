package body Employee is

   function Get_Staff return Employee_T is
   begin
      return 0;
   end Get_Staff;

   function Get_Supervisor return Employee_T is
   begin
      return 0;
   end Get_Supervisor;

   function Get_Manager return Employee_T is
   begin
      return 0;
   end Get_Manager;

   function Last_Name
     (This : Employee_T)
      return String is
   begin
      return "";
   end Last_Name;

   function First_Name
     (This : Employee_T)
      return String is
   begin
      return "";
   end First_Name;

   function Hourly_Rate
     (This : Employee_T)
      return Float is
   begin
      return 0.0;
   end Hourly_Rate;

   function Project
     (This : Employee_T)
      return String is
   begin
      return "";
   end Project;

   function Department
     (This : Employee_T)
      return String is
   begin
      return "";
   end Department;

   function Staff_Count
     (This : Employee_T)
      return String is
   begin
      return "";
   end Staff_Count;

end Employee;
