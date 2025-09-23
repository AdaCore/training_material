with Ada.Text_IO; use Ada.Text_IO;

package body Employee is

   function Image (Date : Date_T) return String is
      Temp_String : String := Date.Year'Image & " -" & Date.Month'Image & " -" & Date.Day'Image;
   begin
      return Temp_String;
   end Image;

   procedure Set_Name (O     : in out Person_T;
                       Value :        Name_T) is
   begin
      O.The_Name := Value;
   end Set_Name;

   function Name (O : Person_T) return Name_T is
   begin
      return O.The_Name;
   end Name;

   procedure Set_Birth_Date (O     : in out Person_T;
                             Value :        Date_T) is
   begin
      O.The_Birth_Date := Value;
   end Set_Birth_Date;

   function Birth_Date (O : Person_T) return Date_T is
   begin
      return O.The_Birth_Date;
   end Birth_Date;

   procedure Print (O : Person_T) is
   begin
      Put_Line ("Name: " & O.Name);
      Put_Line ("Birthdate: " & Image (O.Birth_Date));
   end Print;

   procedure Set_Start_Date
     (O     : in out Employee_T;
      Value :        Date_T) is
   begin
      O.The_Start_Date := Value;
   end Set_Start_Date;

   function Start_Date (O : Employee_T) return Date_T is
   begin
      return O.The_Start_Date;
   end Start_Date;

   procedure Print (O : Employee_T) is
   begin
      Put_Line ("Name: " & Name (O));
      Put_Line ("Birthdate: " & Image (O.Birth_Date));
      Put_Line ("Startdate: " & Image (O.Start_Date));
   end Print;

   procedure Set_Job (O     : in out Position_T;
                      Value :        Job_T) is
   begin
      O.The_Job := Value;
   end Set_Job;

   function Job (O : Position_T) return Job_T is
   begin
      return O.The_Job;
   end Job;

   procedure Print (O : Position_T) is
   begin
      Put_Line ("Name: " & O.Name);
      Put_Line ("Birthdate: " & Image (O.Birth_Date));
      Put_Line ("Startdate: " & Image (O.Start_Date));
      Put_Line ("Job: " & O.Job'Image);
   end Print;

end Employee;
