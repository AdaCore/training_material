----------------------
Ada.Text_IO Lab
----------------------
   
* Requirements
   
   - Create an enumerated type
   - Use the console to query the user how many inputs (N) will follow
   - Use the console to query the user N times for an enumeral
   - If the enumeral is valid, write the index and enumeral to a file

      * Else write an error message to the console

   - When all inputs were read, echo the file to the console

* Hints

   - Use instantiations of the type-specific I/O packages to handle console queries

      * Better error handling

   - Use Text_IO to echo the file to the console
 
----------------------------------------------
Ada.Text_IO Lab Solution
----------------------------------------------

.. code:: Ada

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Main is
      type Enumerated_T is (Red, Yellow, Green);
      package Enum_Io is new Enumeration_IO (Enumerated_T);
      type Count_T is mod 10;
      package Count_Io is new Modular_Io (Count_T);
      E    : Enumerated_T;
      C    : Count_T;
      File : File_Type;
   begin

      Put ("Count: ");
      Count_Io.Get (C);

      Create (File, Out_File, "foo.txt");

      for I in 1 .. C loop
         Count_Io.Put (I, Width => 3);
         Count_Io.Put (File, I, Width => 3);
         Put (" => ");
         begin
            Enum_Io.Get (E);
            Enum_Io.Put (File, E, Width => 10);
         exception
            when Data_Err : others =>
               Put_Line ("Something didn't look right");
         end;
         New_Line (File);
      end loop;
      Close (File);

      Put_Line ("Echoing file");
      Open (File, In_File, "foo.txt");
      while not End_Of_File (File) loop
         Put_Line (Get_Line (File));
      end loop;

   end Main;

