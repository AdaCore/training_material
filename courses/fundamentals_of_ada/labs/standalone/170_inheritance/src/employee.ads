with Ada.Calendar;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package Employee is

  type Person_T is tagged private;
  procedure Set_Name
   (O     : in out Person_T;
    Value :        String);
  function Name
   (O : Person_T)
    return String;
  procedure Set_Birth_Date
   (O     : in out Person_T;
    Value :        String);
  function Birth_Date
   (O : Person_T)
    return String;
  procedure Print (O : Person_T);

  type Employee_T is new Person_T with private;
  not overriding function Start_Date
   (O : Employee_T)
    return String;
  not overriding procedure Set_Start_Date
   (O     : in out Employee_T;
    Value :        String);

  overriding procedure Print (O : Employee_T);

  type Position_T is new Employee_T with private;
  not overriding procedure Set_Job
   (O     : in out Position_T;
    Value :        String);

  not overriding function Job
   (O : Position_T)
    return String;
  overriding procedure Print (O : Position_T);

private
  type Person_T is tagged record
    Name       : Unbounded_String;
    Birth_Date : Ada.Calendar.Time;
  end record;

  type Employee_T is new Person_T with record
    Employee_Id : Positive;
    Start_Date  : Ada.Calendar.Time;
  end record;

  type Position_T is new Employee_T with record
    Job : Unbounded_String;
  end record;

end Employee;
