with Ada.Text_IO; use Ada.Text_IO;
with Employee;
procedure Main is

  function Read
   (Prompt : String)
    return String is
  begin
    Put (Prompt & "> ");
    return Get_Line;
  end Read;

  function Read_Date
   (Prompt : String)
    return String is (Read (Prompt & " (YYYY-MM-DD)"));

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
