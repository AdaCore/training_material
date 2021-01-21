
with Vstring;
package Employee is

   type Category_T is (Staff, Supervisor, Manager);
   type Pay_T is delta 0.01 range 0.0 .. 1_000.00;

   type Employee_T (Category : Category_T := Staff) is record
      Last_Name   : Vstring.Vstring_T;
      First_Name  : Vstring.Vstring_T;
      Hourly_Rate : Pay_T;
      case Category is
         when Staff =>
            null;
         when Supervisor =>
            Project : Vstring.Vstring_T;
         when Manager =>
            Department  : Vstring.Vstring_T;
            Staff_Count : Natural;
      end case;
   end record;

   function Get_Staff return Employee_T;
   function Get_Supervisor return Employee_T;
   function Get_Manager return Employee_T;

end Employee;
