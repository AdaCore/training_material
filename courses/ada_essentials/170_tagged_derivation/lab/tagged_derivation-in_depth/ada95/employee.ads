package Employee is
   type Person_T is tagged private;
   subtype Name_T is String (1 .. 6);
   type Date_T is record
      Year  : Positive;
      Month : Positive;
      Day   : Positive;
   end record;
   type Job_T is (Sales, Engineer, Bookkeeping);

   procedure Set_Name
     (Item  : in out Person_T;
      Value :        Name_T);
   function Name
     (Item : Person_T)
      return Name_T;
   procedure Set_Birth_Date
     (Item  : in out Person_T;
      Value :        Date_T);
   function Birth_Date
     (Item : Person_T)
      return Date_T;
   procedure Print (Item : Person_T);

   type Employee_T is new Person_T with private;
   procedure Set_Start_Date
     (Item  : in out Employee_T;
      Value :        Date_T);
   function Start_Date
     (Item : Employee_T)
      return Date_T;
   procedure Print (Item : Employee_T);

   type Position_T is new Employee_T with private;
   procedure Set_Job
     (Item  : in out Position_T;
      Value :        Job_T);
   function Job
     (Item : Position_T)
      return Job_T;
   procedure Print (Item : Position_T);

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
