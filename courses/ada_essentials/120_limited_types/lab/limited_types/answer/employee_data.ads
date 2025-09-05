package Employee_Data is

   subtype Name_T is String (1 .. 6);
   type Employee_T is limited private;
   type Hourly_Rate_T is delta 0.01 digits 6 range 0.0 .. 999.99;
   type Id_T is range 999 .. 9_999;

   function Create
     (Name : Name_T; Rate : Hourly_Rate_T := 0.0) return Employee_T;
   function Id (Employee : Employee_T) return Id_T;
   function Name (Employee : Employee_T) return Name_T;
   function Rate (Employee : Employee_T) return Hourly_Rate_T;

private
   type Employee_T is limited record
      Name : Name_T := (others => ' ');
      Rate : Hourly_Rate_T := 0.0;
      Id   : Id_T := Id_T'First;
   end record;
end Employee_Data;
