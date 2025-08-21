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
         case Component (Operator, 1) is
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
