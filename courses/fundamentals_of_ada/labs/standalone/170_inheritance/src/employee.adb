with Ada.Calendar.Formatting;
with Ada.Text_IO; use Ada.Text_IO;
package body Employee is

  subtype Month_T is String (1 .. 3);
  Month_Name : array (Ada.Calendar.Month_Number) of Month_T :=
   (1 => "Jan", 2 => "Feb", 3 => "Mar", 4 => "Apr", 5 => "May", 6 => "Jun",
    7 => "Jul", 8 => "Aug", 9 => "Sep", 10 => "Oct", 11 => "Nov", 12 => "Dec");

  function To_String
   (T : Ada.Calendar.Time)
    return String is
  begin
    return Month_Name (Ada.Calendar.Month (T)) &
     Integer'Image (Ada.Calendar.Day (T)) & "," &
     Integer'Image (Ada.Calendar.Year (T));
  end To_String;

  function From_String
   (S : String)
    return Ada.Calendar.Time is
    Date : constant String := S & " 12:00:00";
  begin
    return Ada.Calendar.Formatting.Value (Date);
  end From_String;

  procedure Set_Name
   (O     : in out Person_T;
    Value :        String) is
  begin
    O.Name := To_Unbounded_String (Value);
  end Set_Name;

  function Name
   (O : Person_T)
    return String is (To_String (O.Name));

  procedure Set_Birth_Date
   (O     : in out Person_T;
    Value :        String) is
  begin
    O.Birth_Date := From_String (Value);
  end Set_Birth_Date;

  function Birth_Date
   (O : Person_T)
    return String is (To_String (O.Birth_Date));

  procedure Print (O : Person_T) is
  begin
    Put_Line ("Name: " & Name (O));
    Put_Line ("Birthdate: " & Birth_Date (O));
  end Print;

  not overriding procedure Set_Start_Date
   (O     : in out Employee_T;
    Value :        String) is
  begin
    O.Start_Date := From_String (Value);
  end Set_Start_Date;
  not overriding function Start_Date
   (O : Employee_T)
    return String is (To_String (O.Start_Date));

  overriding procedure Print (O : Employee_T) is
  begin
    Put_Line ("Name: " & Name (O));
    Put_Line ("Birthdate: " & Birth_Date (O));
    Put_Line ("Startdate: " & Start_Date (O));
  end Print;

  not overriding procedure Set_Job
   (O     : in out Position_T;
    Value :        String) is
  begin
    O.Job := To_Unbounded_String (Value);
  end Set_Job;

  not overriding function Job
   (O : Position_T)
    return String is (To_String (O.Job));

  overriding procedure Print (O : Position_T) is
  begin
    Put_Line ("Name: " & Name (O));
    Put_Line ("Birthdate: " & Birth_Date (O));
    Put_Line ("Startdate: " & Start_Date (O));
    Put_Line ("Job: " & Job (O));
  end Print;

end Employee;
