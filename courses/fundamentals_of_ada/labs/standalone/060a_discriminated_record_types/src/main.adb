
with Ada.Text_IO; use Ada.Text_IO;
with Employee;
with Vstring;
use type Vstring.Vstring_T;
procedure Main is

   procedure Print (Member : Employee.Employee_T) is
      First_Line : Vstring.Vstring_T :=
        Member.First_Name & " " & Member.Last_Name & " " &
        Member.Hourly_Rate'Image;
   begin
      Put_Line (Vstring.To_String (First_Line));
      case Member.Category is
         when Employee.Supervisor =>
            Put_Line ("   Project: " & Vstring.To_String (Member.Project));
         when Employee.Manager =>
            Put_Line
              ("   Overseeing " & Member.Staff_Count'Image & " in " &
               Vstring.To_String (Member.Department));
         when others =>
            null;
      end case;
   end Print;

   List  : array (1 .. 1_000) of Employee.Employee_T;
   Count : Natural := 0;

begin

   loop
      Put_Line ("E => Employee");
      Put_Line ("S => Supervisor");
      Put_Line ("M => Manager");
      Put ("E/S/M (any other to stop): ");
      declare
         Choice : constant String := Get_Line;
      begin
         case Choice (1) is
            when 'E' | 'e' =>
               Count        := Count + 1;
               List (Count) := Employee.Get_Staff;
            when 'S' | 's' =>
               Count        := Count + 1;
               List (Count) := Employee.Get_Supervisor;
            when 'M' | 'm' =>
               Count        := Count + 1;
               List (Count) := Employee.Get_Manager;
            when others =>
               exit;
         end case;
      end;
   end loop;

   for Item of List (1 .. Count)
   loop
      Print (Item);
   end loop;

end Main;
