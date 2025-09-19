with Ada.Text_IO; use Ada.Text_IO;
with Employee;    use Employee;
with Vstring;     use Vstring;
procedure Main is
   List  : array (1 .. 1_000) of Employee_T;
   Count : Natural := 0;

   procedure Print (Member : Employee_T) is
   begin
      null;
   end Print;

begin

   --  Create employee(s), supervisor(s), and manager(s)
   --  Add each item to the list

   for Item of List (1 .. Count) loop
      Print (Item);
   end loop;
end Main;
