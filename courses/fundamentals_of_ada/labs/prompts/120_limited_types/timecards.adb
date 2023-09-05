package body Timecards is

   function Create
     (Name  : Employee_Data.Name_T;
      Rate  : Employee_Data.Hourly_Rate_T;
      Hours : Hours_Worked_T)
      return Timecard_T is
   begin
      return Timecard_T'(others => <>);
   end Create;

   function Id
     (Timecard : Timecard_T)
      return Employee_Data.Id_T is
   begin
      return Employee_Data.Id_T'first;
   end Id;

   function Name
     (Timecard : Timecard_T)
      return Employee_Data.Name_T is
   begin
      return "";
   end Name;

   function Rate
     (Timecard : Timecard_T)
      return Employee_Data.Hourly_Rate_T is
   begin
      return Employee_Data.Hourly_Rate_T'first;
   end Rate;

   function Pay
     (Timecard : Timecard_T)
      return Pay_T is
   begin
      return Pay_T'first;
   end Pay;

   function Image
     (Timecard : Timecard_T)
      return String is
   begin
      return "";
   end Image;
end Timecards;
