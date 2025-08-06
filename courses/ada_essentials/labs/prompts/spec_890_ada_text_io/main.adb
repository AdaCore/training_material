with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
   type Enumerated_T is (Red, Yellow, Green);
   -- Create instance to do I/O for Enumerated_T

   type Count_T is mod 10;
   -- Create instance to do I/O for Count_T
   E : Enumerated_T;
   C : Count_T;

begin

   -- Get the count of entries

   -- Create a text file for writing

   --  for the number of entries specified
   -- Print an input prompt
   -- Read the enumeral from the console
   -- Write the count and enumeral to the file
   -- Write an error message to the console if something failed
   --  end loop;

   Put_Line ("Echoing file");
   -- Re-open the file and echo to the console

end Main;
