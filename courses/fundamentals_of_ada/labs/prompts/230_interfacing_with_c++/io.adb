with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Text_IO;
package body Io is
   -- Create import for C's "gets" function
   -- NOTE: "gets" returns the string as both the parameter AND the
   -- return value, so you need to specify at LEAST the parameter
   function Read_From_Stdin return String is
   begin
      -- Call C's "gets" function and convert the result to an Ada string
      return "";
   end Read_From_Stdin;

   -- Create import for C's "puts" function
   procedure Write_To_Stdout (Str : String) is
   begin
      -- Call C's "puts" function to print Str
      null;
   end Write_To_Stdout;

   -- Create import for C's "fopen" function
   function Create_File
     (Filename : String)
      return File_T is
   begin
      -- Call C's "fopen" function to open the file and return the result
      return File_T'(others => <> );
   end Create_File;

   -- Create import for C's "fputs" function
   procedure Write_To_File
     (File : File_T;
      Str  : String) is
   begin
      -- Call C's "fputs" function to print Str to the specified file
      null;
   end Write_To_File;

   -- Create import for C's "fclose" function
   procedure Close_File (File : File_T) is
   begin
      -- Call C's "fclose" function to close the file
      null;
   end Close_File;
end Io;
