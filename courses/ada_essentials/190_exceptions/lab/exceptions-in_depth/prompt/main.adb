with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Debug_Pkg;
with Input;                 use Input;
procedure Main is
   Illegal_Operator : exception;
begin
   loop
      declare
         Input                 : constant String := Get_String ("Sequence");
      begin
         exit when Input'Length = 0;
         -- Parse Input to get the the operator and operands
         --   (handle exceptions as appropriate)
         -- Perform appropriate operation
         --   (handle exceptions as appropriate)
      end;
   end loop;
   Debug_Pkg.Print_Exceptions;
end Main;
