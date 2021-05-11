with Ada.Exceptions;
with Io; use Io;
procedure Main is

   function Get
     (Prompt : String)
      return String is
   begin
      Io.Write_To_Stdout (Prompt & ">", False);
      return Io.Read_From_Stdin;
   end Get;

begin

   declare
      Filename : constant String := Get ("Filename");
      File_Ptr : Io.File_T       := Io.Create_File (Filename);
   begin
      loop
         declare
            Str : constant String := Get ("Line");
         begin
            exit when Str'Length = 0;
            Io.Write_To_File (File_Ptr, Str, True);
         end;
      end loop;
      Io.Close_File (File_Ptr);
   end;

end Main;
