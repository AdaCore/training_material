-----------------
Inheritance Lab
-----------------

* Requirements

   - Create a type structure that could be used in a business

      - A **person** has some defining characteristics
      - An **employee** is a *person* with some employment information
      - A **staff member** is an *employee* with specific job information

   - Create primitive operations to read and print the objects
   - Create a main program to test the objects and operations

* Hints

   - Use `overriding` and `not overriding` as appropriate

---------------------------------------
Inheritance Lab Solution - Data Types
---------------------------------------
.. code:: Ada

   with Ada.Calendar;
   with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
   package Employee is

     type Person_T is tagged private;
     procedure Set_Name (O     : in out Person_T;
                         Value :        String);
     function Name (O : Person_T) return String;
     procedure Set_Birth_Date (O     : in out Person_T;
                               Value :        String);
     function Birth_Date (O : Person_T) return String;
     procedure Print (O : Person_T);

     type Employee_T is new Person_T with private;
     not overriding procedure Set_Start_Date (O     : in out Employee_T;
                                              Value :        String);
     not overriding function Start_Date (O : Employee_T) return String;
     overriding procedure Print (O : Employee_T);

     type Position_T is new Employee_T with private;
     not overriding procedure Set_Job (O     : in out Position_T;
                                       Value :        String);
     not overriding function Job (O : Position_T) return String;
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

---------------------------------
Inheritance Lab Solution - Main
---------------------------------
.. code:: Ada

   with Ada.Text_IO; use Ada.Text_IO;
   with Employee;
   procedure Main is

     function Read (Prompt : String) return String is
     begin
       Put (Prompt & "> ");
       return Get_Line;
     end Read;

     function Read_Date (Prompt : String) return String is
        (Read (Prompt & " (YYYY-MM-DD)"));

     Applicant : Employee.Person_T;
     Employ    : Employee.Employee_T;
     Staff     : Employee.Position_T;

   begin

     Applicant.Set_Name (Read ("Applicant name"));
     Applicant.Set_Birth_Date (Read_Date ("   Birth Date"));

     Employ.Set_Name (Read ("Employee name"));
     Employ.Set_Birth_Date (Read_Date ("   Birth Date"));
     Employ.Set_Start_Date (Read_Date ("   Start Date"));

     Staff.Set_Name (Read ("Staff name"));
     Staff.Set_Birth_Date (Read_Date ("   Birth Date"));
     Staff.Set_Start_Date (Read_Date ("   Start Date"));
     Staff.Set_Job (Read ("   Job"));

     Applicant.Print;
     Employ.Print;
     Staff.Print;

   end Main;
