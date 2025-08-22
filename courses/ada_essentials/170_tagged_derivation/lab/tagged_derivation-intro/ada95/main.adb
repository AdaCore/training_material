with Ada.Text_IO; use Ada.Text_IO;
with Employee;

procedure Main is
   Applicant : Employee.Person_T;
   Employ    : Employee.Employee_T;
   Staff     : Employee.Position_T;

   Birth_Date_1 : Employee.Date_T := (Year  => 1_234, Month => 12, Day   => 1);
   Birth_Date_2 : Employee.Date_T := (Year  => 2_345, Month => 11, Day   => 2);
   Start_Date_2 : Employee.Date_T := (Year  => 3_456, Month => 10, Day   => 3);
   Birth_Date_3 : Employee.Date_T := (Year  => 4_567, Month => 9,  Day   => 4);
   Start_Date_3 : Employee.Date_T := (Year  => 5_678, Month => 8,  Day   => 5);

begin
   Applicant.Set_Name ("Wilma ");
   Applicant.Set_Birth_Date (Birth_Date_1);

   Employ.Set_Name ("Betty ");
   Employ.Set_Birth_Date (Birth_Date_2);
   Employ.Set_Start_Date (Start_Date_2);

   Staff.Set_Name ("Bambam");
   Staff.Set_Birth_Date (Birth_Date_3);
   Staff.Set_Start_Date (Start_Date_3);
   Staff.Set_Job (Employee.Engineer);

   Put_Line ("--- Applicant Details ---");
   Applicant.Print;
   New_Line;

   Put_Line ("--- Employee Details ---");
   Employ.Print;
   New_Line;

   Put_Line ("--- Staff Position Details ---");
   Staff.Print;
   New_Line;

end Main;
