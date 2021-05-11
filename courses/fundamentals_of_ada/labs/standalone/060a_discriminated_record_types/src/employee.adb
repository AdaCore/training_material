
with Ada.Text_IO; use Ada.Text_IO;
with Vstring;     use Vstring;
package body Employee is

   function Read
     (Prompt : String)
      return String is
   begin
      Put (Prompt & " > ");
      return Get_Line;
   end Read;

   function Get_Staff return Employee_T is
      Ret_Val : Employee_T (Staff);
   begin
      Ret_Val.Last_Name   := To_Vstring (Read ("Last name"));
      Ret_Val.First_Name  := To_Vstring (Read ("First name"));
      Ret_Val.Hourly_Rate := Pay_T'Value (Read ("Hourly rate"));
      return Ret_Val;
   end Get_Staff;

   function Get_Supervisor return Employee_T is
      Ret_Val : Employee_T (Supervisor);
   begin
      Ret_Val.Last_Name   := To_Vstring (Read ("Last name"));
      Ret_Val.First_Name  := To_Vstring (Read ("First name"));
      Ret_Val.Hourly_Rate := Pay_T'Value (Read ("Hourly rate"));
      Ret_Val.Project     := To_Vstring (Read ("Project"));
      return Ret_Val;
   end Get_Supervisor;

   function Get_Manager return Employee_T is
      Ret_Val : Employee_T (Manager);
   begin
      Ret_Val.Last_Name   := To_Vstring (Read ("Last name"));
      Ret_Val.First_Name  := To_Vstring (Read ("First name"));
      Ret_Val.Hourly_Rate := Pay_T'Value (Read ("Hourly rate"));
      Ret_Val.Department  := To_Vstring (Read ("Department"));
      Ret_Val.Staff_Count := Integer'Value (Read ("Staff count"));
      return Ret_Val;
   end Get_Manager;

end Employee;
