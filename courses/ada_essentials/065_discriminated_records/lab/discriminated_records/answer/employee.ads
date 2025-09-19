with Vstring; use Vstring;
package Employee is
   type Category_T is (Staff, Supervisor, Manager);
   type Pay_T is delta 0.25 range 0.0 .. 1_000.00;

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
