with Ada.Text_IO; use Ada.Text_IO;

procedure Main is

   type Digit_T is range 0 .. 9;
   type Digit_Name_T is
     (Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine);

   function Convert (Value : Digit_Name_T) return Digit_T;
   function Convert (Value : Character) return Digit_Name_T;
   function Convert (Value : String) return Digit_T;

   function "=" (L : Digit_Name_T; R : Digit_T) return Boolean is
   begin
      return Convert (L) = R;
   end "=";

   function Convert (Value : Digit_Name_T) return Digit_T is
   begin
      case Value is
         when Zero =>
            return 0;

         when One =>
            return 1;

         when Two =>
            return 2;

         when Three =>
            return 3;

         when Four =>
            return 4;

         when Five =>
            return 5;

         when Six =>
            return 6;

         when Seven =>
            return 7;

         when Eight =>
            return 8;

         when Nine =>
            return 9;
      end case;
   end Convert;

   function Convert (Value : Character) return Digit_Name_T is
   begin
      case Value is
         when '0' =>
            return Zero;

         when '1' =>
            return One;

         when '2' =>
            return Two;

         when '3' =>
            return Three;

         when '4' =>
            return Four;

         when '5' =>
            return Five;

         when '6' =>
            return Six;

         when '7' =>
            return Seven;

         when '8' =>
            return Eight;

         when '9' =>
            return Nine;

         when others =>
            return Zero;
      end case;
   end Convert;

   function Convert (Value : String) return Digit_T is
   begin
      return Convert (Digit_Name_T'Value (Value));
   end Convert;

   function Get_Line return String is
      S : String (1 .. 100);
      L : Integer;
   begin
      Get_Line (S, L);
      return S (1 .. L);
   end Get_Line;

   Last_Entry : Digit_T := 0;

begin
   loop
      Put ("Input: ");
      declare
         Str : constant String := Get_Line;
      begin
         exit when Str'Length = 0;
         if Str (Str'First) in '0' .. '9' then
            declare
               Converted : constant Digit_Name_T := Convert (Str (Str'First));
            begin
               Put (Digit_Name_T'Image (Converted));
               if Converted = Last_Entry then
                  Put_Line (" - same as previous");
               else
                  Last_Entry := Convert (Converted);
                  New_Line;
               end if;
            end;
         else
            declare
               Converted : constant Digit_T := Convert (Str);
            begin
               Put (Digit_T'Image (Converted));
               if Converted = Last_Entry then
                  Put_Line (" - same as previous");
               else
                  Last_Entry := Converted;
                  New_Line;
               end if;
            end;
         end if;
      end;
   end loop;
end Main;
