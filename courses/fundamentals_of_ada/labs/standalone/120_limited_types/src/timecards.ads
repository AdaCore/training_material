with Employee_Data;
package Timecards is

  type Hours_Worked_T is digits 3 range 0.0 .. 24.0;
  type Pay_T is digits 6;
  type Timecard_T is limited private;

  function Create
   (Name  : String;
    Rate  : Employee_Data.Hourly_Rate_T;
    Hours : Hours_Worked_T)
    return Timecard_T;

  function Id
   (Timecard : Timecard_T)
    return Employee_Data.Id_T;
  function Name
   (Timecard : Timecard_T)
    return String;
  function Rate
   (Timecard : Timecard_T)
    return Employee_Data.Hourly_Rate_T;
  function Pay
   (Timecard : Timecard_T)
    return Pay_T;

   function image ( timecard : timecard_t ) return string;

private

  type Timecard_T is limited record
    Employee     : Employee_Data.Employee_T;
    Hours_Worked : Hours_Worked_T := 0.0;
    Pay          : Pay_T          := 0.0;
  end record;

end Timecards;
