package Employee_Data is

   type Employee_T is limited private;
   subtype Name_T is String;
   type Hourly_Rate_T is new Integer; -- better implementation
   type Id_T is new Integer;          -- better implementation

   function Create (Name : Name_T; Rate : Hourly_Rate_T) return Employee_T;
   function Id (Employee : Employee_T) return Id_T;
   function Name (Employee : Employee_T) return Name_T;
   function Rate (Employee : Employee_T) return Hourly_Rate_T;

private
   -- finish implementation
   type Employee_T is limited null record;
end Employee_Data;
