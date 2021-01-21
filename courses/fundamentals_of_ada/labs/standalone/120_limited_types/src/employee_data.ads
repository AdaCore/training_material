with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package Employee_Data is

  type Employee_T is limited private;

  type Hourly_Rate_T is delta 0.01 digits 6 range 0.0 .. 999.99;
  type Id_T is range 999 .. 9_999;

  function Create
   (Name : String;
    Rate : Hourly_Rate_T := 0.0)
    return Employee_T;
  function Id
   (Employee : Employee_T)
    return Id_T;
  function Name
   (Employee : Employee_T)
    return String;
  function Rate
   (Employee : Employee_T)
    return Hourly_Rate_T;

private

  type Employee_T is limited record
    Name : Unbounded_String := Null_Unbounded_String;
    Rate : Hourly_Rate_T    := 0.0;
    Id   : Id_T             := Id_T'First;
  end record;

end Employee_Data;
