package body Employee_Data is

   function Create
     (Name : Name_T;
      Rate : Hourly_Rate_T)
      return Employee_T is
   begin
      -- Return an employee object with appropriate data
      return Employee_T'(others => <>);
   end Create;

   function Id
     (Employee : Employee_T)
      return Id_T is
   begin
      -- return ID member of record
      return Id_T'First;
   end Id;

   function Name
     (Employee : Employee_T)
      return Name_T is
   begin
      -- return Name member of record
      return "";
   end Name;

   function Rate
     (Employee : Employee_T)
      return Hourly_Rate_T is
   begin
      -- return Rate member of record;
      return Hourly_Rate_T'First;
   end Rate;

end Employee_Data;
