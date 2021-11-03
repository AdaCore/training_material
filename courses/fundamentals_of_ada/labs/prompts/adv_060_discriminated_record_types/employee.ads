with Ada.Text_IO; use Ada.Text_IO;
with Vstring;     use Vstring;
package Employee is

   type Employee_T is private;

   function Get_Staff return Employee_T;
   function Get_Supervisor return Employee_T;
   function Get_Manager return Employee_T;

   function Last_Name
     (This : Employee_T)
      return String;
   function First_Name
     (This : Employee_T)
      return String;
   function Hourly_Rate
     (This : Employee_T)
      return Float;
   function Project
     (This : Employee_T)
      return String;
   function Department
     (This : Employee_T)
      return String;
   function Staff_Count
     (This : Employee_T)
      return String;

private

   -- implement this as a variant record
   type Employee_T is new Integer;

end Employee;
