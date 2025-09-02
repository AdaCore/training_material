with Employee;
procedure Main is
   Applicant : Employee.Person_T;
   Employ    : Employee.Employee_T;
   Staff     : Employee.Position_T;

begin
   Employee.Set_Name (Applicant, "Wilma ");
   Employee.Set_Birth_Date
     (Applicant,
     (Year   => 1_234,
       Month => 12,
       Day   => 1));

   Employee.Set_Name (Employ, "Betty ");
   Employee.Set_Birth_Date
     (Employ,
     (Year   => 2_345,
       Month => 11,
       Day   => 2));
   Employee.Set_Start_Date
     (Employ,
     (Year   => 3_456,
       Month => 10,
       Day   => 3));

   Employee.Set_Name (Staff, "Bambam");
   Employee.Set_Birth_Date
     (Staff,
     (Year   => 4_567,
       Month => 9,
       Day   => 4));
   Employee.Set_Start_Date
     (Staff,
     (Year   => 5_678,
       Month => 8,
       Day   => 5));
   Employee.Set_Job (Staff, Employee.Engineer);

   Employee.Print (Applicant);
   Employee.Print (Employ);
   Employee.Print (Staff);
end Main;
