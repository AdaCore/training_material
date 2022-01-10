with Ada.Text_IO; use Ada.Text_IO;
procedure Main is

   -- hard-coded filename
   Filename : constant String := "main.adb";

   File : File_Type;
   Line : String (1 .. 100);
   Last : Natural;

   -- store object names here

   Comments   : Natural := 0;
   Semicolons : Natural := 0;

begin

   Open (File, In_File, Filename);
   while not End_Of_File (File) loop
      Get_Line (File, Line, Last);
      -- Count comments
      -- Count semicolons
      -- Store the object name if one is found
   end loop;
   Close (File);

   Put_Line ("Comments: " & Integer'image (Comments));
   Put_Line ("Semi-colons: " & Integer'image (Semicolons));
   Put_Line ("Objects: ");
   -- Print objects
end Main;
