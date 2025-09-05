with Ada.Exceptions;
with Ada.Text_IO;
use type Ada.Exceptions.Exception_Id;

package body Debug_Pkg is
   Exceptions     : array (1 .. 100) of Ada.Exceptions.Exception_Occurrence;
   Next_Available : Integer := 1;
   procedure Save_Occurrence (X : Ada.Exceptions.Exception_Occurrence) is
   begin
      Ada.Exceptions.Save_Occurrence (Exceptions (Next_Available), X);
      Next_Available := Next_Available + 1;
   end Save_Occurrence;
   procedure Print_Exceptions is
   begin
      for I in 1 .. Next_Available - 1 loop
         declare
            E    : Ada.Exceptions.Exception_Occurrence renames Exceptions (I);
            Flag : Character := ' ';
         begin
            if Ada.Exceptions.Exception_Identity (E)
              = Constraint_Error'Identity
            then
               Flag := '*';
            end if;
            Ada.Text_IO.Put_Line
              (Flag & " " & Ada.Exceptions.Exception_Information (E));
         end;
      end loop;
   end Print_Exceptions;
end Debug_Pkg;
