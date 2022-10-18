.. code:: ada

   procedure Aggregates is
   
      type Date_T is record
         Day   : Integer range 1 .. 31;
         Month : Positive range 1 .. 12;
         Year  : Natural range 0 .. 2_099;
      end record;

      type Personal_Information_T is record
         Name      : String (1 .. 10);
         Birthdate : Date_T;
      end record;

      type Employee_Information_T is record
         Number               : Positive;
         Personal_Information : Personal_Information_T;
      end record;

      Birthdate            : Date_T;
      Personal_Information : Personal_Information_T;
      Employee             : Employee_Information_T;
   begin
      [...]
