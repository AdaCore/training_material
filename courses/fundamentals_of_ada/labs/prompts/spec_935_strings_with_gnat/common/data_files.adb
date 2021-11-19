with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Regpat;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Data_Files is

    Re_Abs_Path_Start : constant GNAT.Regpat.Pattern_Matcher
      := GNAT.Regpat.Compile("^(/|[A-Z]+:\\)");
    
    function Absolute_Path (Path : String) return String is
    begin
       if GNAT.Regpat.Match (Re_Abs_Path_Start, Path) /= 0 then
          return Path;
       else
          --  Absolute from the CWD
          return Get_Current_Dir & Path;
       end if;
    end Absolute_Path;

    function Exe_Path return String is
    begin
       return Dir_Name (Absolute_Path (Ada.Command_Line.Command_Name));
    end Exe_Path;

    function Parent (Path : String; Level : Positive) return String is
       P : Unbounded_String := To_Unbounded_String (Path);
    begin
       for J in 1 .. Level - 1 loop
          if Element (Tail (P, 1), 1) = Dir_Separator then
             Delete (P, Length (P), Length (P));
          end if;

          P := To_Unbounded_String (Dir_Name (To_String (P)));
       end loop;

       return To_String (P);
    end Parent;

    Data_Files_Path : constant String
      := Parent (Exe_Path, 2) & "data_files" & Dir_Separator;
    
    function Read (File_Name : String) return Unbounded_String is
       F : File_Type;
       S : Unbounded_String;
    begin

       Open (F, In_File, Data_Files_Path & File_Name);
       begin
          loop
             Append (S, Unbounded_String'(Get_Line (F)));
             Append (S, ' ');
          end loop;
       exception
          when End_Error => null; -- UGLY
       end;
       Close (F);

       return S;
    end Read;

end Data_Files;
