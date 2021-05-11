with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package body Timecards is

  function Create
   (Name  : String;
    Rate  : Employee_Data.Hourly_Rate_T;
    Hours : Hours_Worked_T)
    return Timecard_T is
  begin

    return (Employee => Employee_Data.Create (Name, Rate),
      Hours_Worked   => Hours, Pay => Pay_T (Hours) * Pay_T (Rate));
  end Create;

  function Id
   (Timecard : Timecard_T)
    return Employee_Data.Id_T is (Employee_Data.Id (Timecard.Employee));
  function Name
   (Timecard : Timecard_T)
    return String is (Employee_Data.Name (Timecard.Employee));
  function Rate
   (Timecard : Timecard_T)
    return Employee_Data.Hourly_Rate_T is
   (Employee_Data.Rate (Timecard.Employee));
  function Pay
   (Timecard : Timecard_T)
    return Pay_T is (Timecard.Pay);

  function Image
   (Timecard : Timecard_T)
    return String is
    Name_S : constant String := Name (Timecard);
    Id_S   : constant String :=
     Employee_Data.Id_T'Image (Employee_Data.Id (Timecard.Employee));
    Rate_S : constant String :=
     Employee_Data.Hourly_Rate_T'Image
      (Employee_Data.Rate (Timecard.Employee));
    Hours_S : constant String := Hours_Worked_T'Image (Timecard.Hours_Worked);
    Pay_S   : constant String := Pay_T'Image (Timecard.Pay);
  begin
    return Name_S & " ( " & Id_S & " ) => " & Hours_S & " hours * " & Rate_S &
     "/hour = " & Pay_S;
  end Image;

end Timecards;
