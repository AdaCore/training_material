--Types_Body
with Ada.Text_IO; use Ada.Text_IO;
package body Employee is

   function Image (Date : Date_T) return String is
     (Date.Year'Image & " -" & Date.Month'Image & " -" & Date.Day'Image);

   procedure Set_Name (O     : in out Person_T;
                       Value :        Name_T) is
   begin
      O.The_Name := Value;
   end Set_Name;
   function Name (O : Person_T) return Name_T is (O.The_Name);

   procedure Set_Birth_Date (O     : in out Person_T;
                             Value :        Date_T) is
   begin
      O.The_Birth_Date := Value;
   end Set_Birth_Date;
   function Birth_Date (O : Person_T) return Date_T is (O.The_Birth_Date);

   procedure Print (O : Person_T) is
   begin
      Put_Line ("Name: " & O.Name);
      Put_Line ("Birthdate: " & Image (O.Birth_Date));
   end Print;

   not overriding procedure Set_Start_Date
     (O     : in out Employee_T;
      Value :        Date_T) is
   begin
      O.The_Start_Date := Value;
   end Set_Start_Date;
   not overriding function Start_Date (O : Employee_T) return Date_T is
      (O.The_Start_Date);

   overriding procedure Print (O : Employee_T) is
   begin
      Print (Person_T(O));
      Put_Line ("Startdate: " & Image (O.Start_Date));
   end Print;

--Types_Body
   not overriding procedure Set_Job (O     : in out Position_T;
                                     Value :        Job_T) is
   begin
      O.The_Job := Value;
   end Set_Job;
   not overriding function Job (O : Position_T) return Job_T is
      (O.The_Job);

   overriding procedure Print (O : Position_T) is
   begin
      Put_Line ("Name: " & O.Name);
      Put_Line ("Birthdate: " & Image (O.Birth_Date));
      Put_Line ("Startdate: " & Image (O.Start_Date));
      Put_Line ("Job: " & O.Job'Image);
   end Print;

end Employee;
