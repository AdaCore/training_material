.. code:: ada

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Slices is
      procedure Explicit_Indexes is
         Full_Name : String (1 .. 20) := "Barney    Rubble    ";
      begin
         Put_Line (Full_Name);
         Full_Name (1 .. 10) := "Betty     ";
         Put_Line (Full_Name (1 .. 10)); -- first half of name
         Put_Line (Full_Name (11 .. 20)); -- second half of name
      end Explicit_Indexes;
   
      procedure Subtype_Indexes is
         subtype First_Name is Positive range 1 .. 10;
         subtype Last_Name is Positive range 11 .. 20;
         Full_Name : String (First_Name'First .. Last_Name'Last) :=
           "Fred      Flintstone";
      begin
         Put_Line (Full_Name);
         Full_Name (First_Name) := "Wilma     ";
         Put_Line (Full_Name (First_Name)); -- first half of name
         Put_Line (Full_Name (Last_Name)); -- second half of name
      end Subtype_Indexes;
   begin
      Explicit_Indexes;
      Subtype_Indexes;
   end Slices;
