
with Ada.Text_IO; use Ada.Text_IO;
with Employee;
procedure Main is
  function Read (Prompt : String) return String is
  begin
    Put (Prompt & "> ");
    return Get_Line;
  end Read;
  function Read_Date (Prompt : String) return String is (Read (Prompt & " (YYYY-MM-DD)"));

   Applicant : Employee.Person_T;
   -- Create objects to store information for an Employee and Position

begin
   Employee.Set_Attribute ( Applicant, Read ("Attribute" ) );
   Employee.Print (Applicant );
   -- set attributes / print contents for employee and position
end Main;

