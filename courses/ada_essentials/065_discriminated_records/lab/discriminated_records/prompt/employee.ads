with Vstring; use Vstring;
package Employee is

   type Pay_T is new Integer; --  Needs refinement

   --  Create a type to distinguish regular employees
   --  from supervisors and managers
   --     Everyone has first and last name and pay rate
   --     Supervisors adds a project to a regular employee
   --     Managers add a department and number of employees
   --        being managed to a regular employee

   --  Make this a discriminated record
   type Employee_T is null record;

   function Create_Staff (First_Name  : String;
                          Last_Name   : String;
                          Hourly_Rate : Pay_T)
                          return Employee_T;
   function Create_Supervisor (First_Name  : String;
                               Last_Name   : String;
                               Hourly_Rate : Pay_T;
                               Project     : String)
                               return Employee_T;
   function Create_Manager (First_Name  : String;
                            Last_Name   : String;
                            Hourly_Rate : Pay_T;
                            Department  : String;
                            Staff_Count : Natural)
                            return Employee_T;
end Employee;
