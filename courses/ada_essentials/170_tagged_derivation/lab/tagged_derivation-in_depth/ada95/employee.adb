with Ada.Text_IO; use Ada.Text_IO;
package body Employee is

   function Image
     (Date : Date_T)
      return String is
   begin
      return
        Positive'Image (Date.Year) & " -" & Positive'Image (Date.Month) &
        " -" & Positive'Image (Date.Day);
   end Image;

   procedure Set_Name
     (This  : in out Person_T;
      Value :        Name_T) is
   begin
      This.The_Name := Value;
   end Set_Name;
   function Name
     (This : Person_T)
      return Name_T is
   begin
      return This.The_Name;
   end Name;

   procedure Set_Birth_Date
     (This  : in out Person_T;
      Value :        Date_T) is
   begin
      This.The_Birth_Date := Value;
   end Set_Birth_Date;
   function Birth_Date
     (This : Person_T)
      return Date_T is
   begin
      return This.The_Birth_Date;
   end Birth_Date;

   procedure Print (This : Person_T) is
   begin
      Put_Line ("Name: " & Name (This));
      Put_Line ("Birthdate: " & Image (Birth_Date (This)));
   end Print;

   procedure Set_Start_Date
     (This  : in out Employee_T;
      Value :        Date_T) is
   begin
      This.The_Start_Date := Value;
   end Set_Start_Date;
   function Start_Date
     (This : Employee_T)
      return Date_T is
   begin
      return This.The_Start_Date;
   end Start_Date;

   procedure Print (This : Employee_T) is
   begin
      Print (Person_T (This)); --  Use parent "Print"
      Put_Line ("Startdate: " & Image (Start_Date (This)));
   end Print;

   procedure Set_Job
     (This  : in out Position_T;
      Value :        Job_T) is
   begin
      This.The_Job := Value;
   end Set_Job;
   function Job
     (This : Position_T)
      return Job_T is
   begin
      return This.The_Job;
   end Job;

   procedure Print (This : Position_T) is
   begin
      Put_Line ("Name: " & Name (This));
      Put_Line ("Birthdate: " & Image (Birth_Date (This)));
      Put_Line ("Startdate: " & Image (Start_Date (This)));
      Put_Line ("Job: " & Job_T'Image (Job (This)));
   end Print;

end Employee;
