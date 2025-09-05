with Ada.Text_IO; use Ada.Text_IO;

package body Employee is

   function Image (Date : Date_T) return String is
   begin
      return
        Positive'Image (Date.Year)
        & " -"
        & Positive'Image (Date.Month)
        & " -"
        & Positive'Image (Date.Day);
   end Image;

   procedure Set_Name (Item : in out Person_T; Value : Name_T) is
   begin
      Item.The_Name := Value;
   end Set_Name;
   function Name (Item : Person_T) return Name_T is
   begin
      return Item.The_Name;
   end Name;

   procedure Set_Birth_Date (Item : in out Person_T; Value : Date_T) is
   begin
      Item.The_Birth_Date := Value;
   end Set_Birth_Date;
   function Birth_Date (Item : Person_T) return Date_T is
   begin
      return Item.The_Birth_Date;
   end Birth_Date;

   procedure Print (Item : Person_T) is
   begin
      Put_Line ("Name: " & Name (Item));
      Put_Line ("Birthdate: " & Image (Birth_Date (Item)));
   end Print;

   procedure Set_Start_Date (Item : in out Employee_T; Value : Date_T) is
   begin
      Item.The_Start_Date := Value;
   end Set_Start_Date;
   function Start_Date (Item : Employee_T) return Date_T is
   begin
      return Item.The_Start_Date;
   end Start_Date;

   procedure Print (Item : Employee_T) is
   begin
      Print (Person_T (Item)); --  Use parent "Print"
      Put_Line ("Startdate: " & Image (Start_Date (Item)));
   end Print;

   procedure Set_Job (Item : in out Position_T; Value : Job_T) is
   begin
      Item.The_Job := Value;
   end Set_Job;
   function Job (Item : Position_T) return Job_T is
   begin
      return Item.The_Job;
   end Job;

   procedure Print (Item : Position_T) is
   begin
      Put_Line ("Name: " & Name (Item));
      Put_Line ("Birthdate: " & Image (Birth_Date (Item)));
      Put_Line ("Startdate: " & Image (Start_Date (Item)));
      Put_Line ("Job: " & Job_T'Image (Job (Item)));
   end Print;

end Employee;
