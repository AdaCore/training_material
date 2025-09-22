with Ada.Text_IO; use Ada.Text_IO;
with Employee;    use Employee;
with Vstring;     use Vstring;
procedure Main is
   List  : array (1 .. 1_000) of Employee_T;
   Count : Natural := 0;

   procedure Print (Member : Employee_T) is
      First_Line : constant Vstring.Vstring_T :=
        Member.First_Name & " " & Member.Last_Name & " " &
        Member.Hourly_Rate'Image;
   begin
      Put_Line (Vstring.To_String (First_Line));
      case Member.Category is
         when Supervisor =>
            Put_Line ("   Project: " & Vstring.To_String (Member.Project));
         when Manager =>
            Put_Line
              ("   Overseeing " & Member.Staff_Count'Image & " in " &
               Vstring.To_String (Member.Department));
         when others =>
            null;
      end case;
   end Print;

   procedure Add (Item : Employee_T) is
   begin
      Count        := Count + 1;
      List (Count) := Item;
   end Add;

begin

   Add (Create_Manager ("Wilma", "Flintstone", 1.23, "Payroll", 4));
   Add (Create_Supervisor ("Christopher", "Pike", 5.67, "Starship"));
   Add (Create_Staff ("Jamie", "Dutton", 8.90));

   for Item of List (1 .. Count) loop
      Print (Item);
   end loop;
end Main;
