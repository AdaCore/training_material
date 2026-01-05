--Types_Body
with Ada.Text_IO; use Ada.Text_IO;
package body Employee is

   function Image (Date : Date_T) return String is
     (Date.Year'Image & " -" & Date.Month'Image & " -" & Date.Day'Image);

   procedure Set_Name (This  : in out Person_T;
                       Value :        Name_T) is
   begin
      This.The_Name := Value;
   end Set_Name;
   function Name (This : Person_T) return Name_T is (This.The_Name);

   procedure Set_Birth_Date (This  : in out Person_T;
                             Value :        Date_T) is
   begin
      This.The_Birth_Date := Value;
   end Set_Birth_Date;
   function Birth_Date (This : Person_T) return Date_T is
      (This.The_Birth_Date);

   procedure Print (This : Person_T) is
   begin
      Put_Line ("Name: " & This.Name);
      Put_Line ("Birthdate: " & Image (This.Birth_Date));
   end Print;

   not overriding procedure Set_Start_Date
     (This  : in out Employee_T;
      Value :        Date_T) is
   begin
      This.The_Start_Date := Value;
   end Set_Start_Date;
   not overriding function Start_Date (This : Employee_T) return Date_T is
      (This.The_Start_Date);

   overriding procedure Print (This : Employee_T) is
   begin
      Print (Person_T(This));
      Put_Line ("Startdate: " & Image (This.Start_Date));
   end Print;

--Types_Body
   not overriding procedure Set_Job (This  : in out Position_T;
                                     Value :        Job_T) is
   begin
      This.The_Job := Value;
   end Set_Job;
   not overriding function Job (This : Position_T) return Job_T is
      (This.The_Job);

   overriding procedure Print (This : Position_T) is
   begin
      Put_Line ("Name: " & This.Name);
      Put_Line ("Birthdate: " & Image (This.Birth_Date));
      Put_Line ("Startdate: " & Image (This.Start_Date));
      Put_Line ("Job: " & This.Job'Image);
   end Print;

end Employee;
