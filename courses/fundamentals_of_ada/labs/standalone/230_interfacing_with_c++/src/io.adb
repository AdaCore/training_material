with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Text_IO;
package body Io is

   function C_Gets return Interfaces.C.Strings.chars_ptr with
      Import,
      Convention    => C,
      External_Name => "gets";
   function Read_From_Stdin return String is
     (Interfaces.C.Strings.Value (C_Gets));

   procedure C_Puts (Str : Interfaces.C.Strings.chars_ptr) with
      Import,
      Convention    => C,
      External_Name => "puts";
   procedure C_Putchar (Str : Interfaces.C.char) with
      Import,
      Convention    => C,
      External_Name => "putchar";
   procedure Write_To_Stdout
     (Str     : String;
      With_Lf : Boolean := True) is
   begin
      -- "puts" always adds a line-feed
      if With_Lf
      then
         declare
            To_C : Interfaces.C.Strings.chars_ptr :=
              Interfaces.C.Strings.New_String (Str);
         begin
            C_Puts (To_C);
            Interfaces.C.Strings.Free (To_C);
         end;
      else
         for C of Str
         loop
            C_Putchar (Interfaces.C.To_C (C));
         end loop;
      end if;
   end Write_To_Stdout;

   function C_Fopen
     (Filename : Interfaces.C.char_array;
      Mode     : Interfaces.C.char_array)
      return File_T with
      Import,
      Convention    => C,
      External_Name => "fopen";
   function Create_File
     (Filename : String)
      return File_T is
     (C_Fopen (Interfaces.C.To_C (Filename), Interfaces.C.To_C ("w")));

   procedure C_Fputs
     (Str  : Interfaces.C.Strings.chars_ptr;
      File : File_T) with
      Import,
      Convention    => C,
      External_Name => "fputs";
   procedure Write_To_File
     (File    : File_T;
      Str     : String;
      With_Lf : Boolean := True) is
      To_C : Interfaces.C.Strings.chars_ptr;
   begin
      if With_Lf
      then
         To_C := Interfaces.C.Strings.New_String (Str & ASCII.LF);
      else
         To_C := Interfaces.C.Strings.New_String (Str);
      end if;
      C_Fputs (To_C, File);
      Interfaces.C.Strings.Free (To_C);
   end Write_To_File;

   procedure C_Fclose (File : File_T) with
      Import,
      Convention    => C,
      External_Name => "fclose";
   procedure Close_File (File : File_T) is
   begin
      C_Fclose (File);
   end Close_File;

end Io;
