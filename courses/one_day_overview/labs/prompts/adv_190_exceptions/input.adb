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
            if Retval'length > 0 then
               return Integer_T'value (Retval);
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
            if Retval'length > 0 then
               return Float_T'value (Retval);
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
      for E in Enum_T'range loop
         Put_Line
           (Integer'image (1 + Enum_T'pos (E)) & "> " & Enum_T'image (E));
      end loop;
      loop
         declare
            I : constant String := Get_String (Prompt);
         begin
            if I'length = 0 then
               raise Input_Canceled;
            end if;
            return Enum_T'val (Natural'value (I) - 1);
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
