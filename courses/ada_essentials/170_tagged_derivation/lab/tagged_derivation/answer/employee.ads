package Employee is
   type Person_T is tagged private;
   subtype Name_T is String (1 .. 6);
   type Date_T is record
      Year  : Positive;
      Month : Positive;
      Day   : Positive;
   end record;
   type Job_T is (Sales, Engineer, Bookkeeping);

   procedure Set_Name (O : in out Person_T; Value : Name_T);
   function Name (O : Person_T) return Name_T;
   procedure Set_Birth_Date (O : in out Person_T; Value : Date_T);
   function Birth_Date (O : Person_T) return Date_T;
   procedure Print (O : Person_T);

   type Employee_T is new Person_T with private;
   not overriding
   procedure Set_Start_Date (O : in out Employee_T; Value : Date_T);
   not overriding
   function Start_Date (O : Employee_T) return Date_T;
   overriding
   procedure Print (O : Employee_T);

   type Position_T is new Employee_T with private;
   not overriding
   procedure Set_Job (O : in out Position_T; Value : Job_T);
   not overriding
   function Job (O : Position_T) return Job_T;
   overriding
   procedure Print (O : Position_T);

private
   type Person_T is tagged record
      The_Name       : Name_T;
      The_Birth_Date : Date_T;
   end record;

   type Employee_T is new Person_T with record
      The_Employee_Id : Positive;
      The_Start_Date  : Date_T;
   end record;

   type Position_T is new Employee_T with record
      The_Job : Job_T;
   end record;
end Employee;
