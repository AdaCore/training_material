with System;
package Io is

   type File_T is limited private;

   function Read_From_Stdin return String;
   procedure Write_To_Stdout
     (Str     : String;
      With_Lf : Boolean := True);

   function Create_File
     (Filename : String)
      return File_T;
   procedure Write_To_File
     (File    : File_T;
      Str     : String;
      With_Lf : Boolean := True);
   procedure Close_File (File : File_T);

private
   type File_T is access all Integer;

end Io;
