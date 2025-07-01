--Calculator_Body
package body Calculator is
   function Value
     (Str : String)
      return Integer_T is
   begin
      return Integer_T'Value (Str);
   exception
      when Constraint_Error =>
         raise Formatting_Error;
   end Value;
   function Add
     (Left, Right : String)
      return Integer_T is
   begin
      return Value (Left) + Value (Right);
   end Add;
   function Subtract
     (Left, Right : String)
      return Integer_T is
   begin
      return Value (Left) - Value (Right);
   end Subtract;
   function Multiply
     (Left, Right : String)
      return Integer_T is
   begin
      return Value (Left) * Value (Right);
   end Multiply;
   function Divide
     (Top, Bottom : String)
      return Integer_T is
   begin
      if Value (Bottom) = 0 then
         raise Divide_By_Zero;
      else
         return Value (Top) / Value (Bottom);
      end if;
   end Divide;
end Calculator;
--Calculator_Body
--Calculator_Spec
package Calculator is
   Formatting_Error : exception;
   Divide_By_Zero   : exception;
   type Integer_T is range -1_000 .. 1_000;
   function Add
     (Left, Right : String)
      return Integer_T;
   function Subtract
     (Left, Right : String)
      return Integer_T;
   function Multiply
     (Left, Right : String)
      return Integer_T;
   function Divide
     (Top, Bottom : String)
      return Integer_T;
end Calculator;
--Calculator_Spec
--Debug
with Ada.Exceptions;
package Debug_Pkg is
   procedure Save_Occurrence (X : Ada.Exceptions.Exception_Occurrence);
   procedure Print_Exceptions;
end Debug_Pkg;

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
            if Ada.Exceptions.Exception_Identity (E) =
              Constraint_Error'Identity
            then
               Flag := '*';
            end if;
            Ada.Text_IO.Put_Line
              (Flag & " " & Ada.Exceptions.Exception_Information (E));
         end;
      end loop;
   end Print_Exceptions;
end Debug_Pkg;
--Debug
with Ada.Text_IO;             use Ada.Text_IO;
package body Input is
   function Get_String
     (Prompt : String)
      return String is
      Str  : String (1 .. 100);
      Last : Integer;
   begin
      Put (Prompt & " > ");
      Ada.Text_IO.Get_Line (Str, Last);
      return Str (1 .. Last);
   end Get_String;
   function Get_Number
     (Prompt : String)
      return Integer_T is
   begin
      loop
         declare
            Retval : constant String := Get_String (Prompt);
         begin
            if Retval'Length > 0 then
               return Integer_T'Value (Retval);
            else
               raise Input_Canceled;
            end if;
         exception
            when others =>
               Put_Line ("Invalid input");
         end;
      end loop;
   end Get_Number;
   function Get_Float
     (Prompt : String)
      return Float_T is
   begin
      loop
         declare
            Retval : constant String := Get_String (Prompt);
         begin
            if Retval'Length > 0 then
               return Float_T'Value (Retval);
            else
               raise Input_Canceled;
            end if;
         exception
            when others =>
               Put_Line ("Invalid input");
         end;
      end loop;
   end Get_Float;
   function Get_Enum
     (Prompt : String)
      return Enum_T is
   begin
      for E in Enum_T'Range loop
         Put_Line
           (Integer'Image (1 + Enum_T'Pos (E)) & "> " & Enum_T'image (E));
      end loop;
      loop
         declare
            I : constant String := Get_String (Prompt);
         begin
            if I'Length = 0 then
               raise Input_Canceled;
            end if;
            return Enum_T'Val (Natural'value (I) - 1);
         exception
            when Input_Canceled =>
               raise Input_Canceled;
            when others =>
               Put_Line ("Illegal value");
         end;
      end loop;
   end Get_Enum;
   function Internal_Get_Integer is new Get_Number (Integer);
   function Internal_Get_Natural is new Get_Number (Natural);
   function Internal_Get_Positive is new Get_Number (Positive);
   function Get_Integer
     (Prompt : String)
      return Integer renames Internal_Get_Integer;
   function Get_Natural
     (Prompt : String)
      return Natural renames Internal_Get_Natural;
   function Get_Positive
     (Prompt : String)
      return Positive renames Internal_Get_Positive;
end Input;
package Input is
   Input_Canceled : exception;
   function Get_String
     (Prompt : String)
      return String;
   function Get_Integer
     (Prompt : String)
      return Integer;
   function Get_Natural
     (Prompt : String)
      return Natural;
   function Get_Positive
     (Prompt : String)
      return Positive;
   generic
      type Enum_T is (<>);
   function Get_Enum
     (Prompt : String)
      return Enum_T;
   generic
      type Integer_T is range <>;
   function Get_Number
     (Prompt : String)
      return Integer_T;
   generic
      type Float_T is digits <>;
   function Get_Float
     (Prompt : String)
      return Float_T;
end Input;
--Main
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Calculator;            use Calculator;
with Debug_Pkg;
with Input;                 use Input;
procedure Main is
   Illegal_Operator : exception;
   procedure Parser
     (Str      :     String;
      Left     : out Unbounded_String;
      Operator : out Unbounded_String;
      Right    : out Unbounded_String) is
      I : Integer := Str'First;
   begin
      while I <= Str'Length and then Str (I) /= ' ' loop
         Left := Left & Str (I);
         I    := I + 1;
      end loop;
      while I <= Str'Length and then Str (I) = ' ' loop
         I := I + 1;
      end loop;
      while I <= Str'Length and then Str (I) /= ' ' loop
         Operator := Operator & Str (I);
         I        := I + 1;
      end loop;
      while I <= Str'Length and then Str (I) = ' ' loop
         I := I + 1;
      end loop;
      while I <= Str'Length and then Str (I) /= ' ' loop
         Right := Right & Str (I);
         I     := I + 1;
      end loop;
   end Parser;
begin
   loop
      declare
         Left, Operator, Right : Unbounded_String;
         Input                 : constant String := Get_String ("Sequence");
      begin
         exit when Input'Length = 0;
         Parser (Input, Left, Operator, Right);
         case Element (Operator, 1) is
            when '+' =>
               Put_Line
                 ("  => " &
                  Integer_T'Image (Add (To_String (Left), To_String (Right))));
            when '-' =>
               Put_Line
                 ("  => " &
                  Integer_T'Image
                    (Subtract (To_String (Left), To_String (Right))));
            when '*' =>
               Put_Line
                 ("  => " &
                  Integer_T'Image
                    (Multiply (To_String (Left), To_String (Right))));
            when '/' =>
               Put_Line
                 ("  => " &
                  Integer_T'Image
                    (Divide (To_String (Left), To_String (Right))));
            when others =>
               raise Illegal_Operator;
         end case;
      exception
         when The_Err : others =>
            Debug_Pkg.Save_Occurrence (The_Err);
      end;
   end loop;
   Debug_Pkg.Print_Exceptions;
end Main;
--Main
