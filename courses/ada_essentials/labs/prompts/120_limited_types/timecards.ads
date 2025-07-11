with Employee_Data;
package Timecards is

   type Hours_Worked_T is new Integer;  -- better implementation
   type Pay_T is new Integer;           -- better implementation
   type Timecard_T is limited private;

   function Create
     (Name  : Employee_Data.Name_T;
      Rate  : Employee_Data.Hourly_Rate_T;
      Hours : Hours_Worked_T)
      return Timecard_T;

   function Id
     (Timecard : Timecard_T)
      return Employee_Data.Id_T;
   function Name
     (Timecard : Timecard_T)
      return Employee_Data.Name_T;
   function Rate
     (Timecard : Timecard_T)
      return Employee_Data.Hourly_Rate_T;
   function Pay
     (Timecard : Timecard_T)
      return Pay_T;
   function Image
     (Timecard : Timecard_T)
      return String;

private
  -- finish implementation
   type Timecard_T is limited null record;

end Timecards;
