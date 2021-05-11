with Ada.Text_IO; use Ada.Text_IO;
procedure Main is

   type Digit_T is range 0 .. 9;
   type Digit_Name_T is
     (Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine);

   function Convert (Value : Digit_T) return Digit_Name_T;
   function Convert (Value : Digit_Name_T) return Digit_T;
   function Convert (Value : Character) return Digit_Name_T;
   function Convert (Value : String) return Digit_T;

   function "=" (L : Digit_T;
                 R : Digit_Name_T)
                 return Boolean is
      (Convert (L) = R);
   function "=" (L : Digit_Name_T;
                 R : Digit_T)
                 return Boolean is
      (Convert (L) = R);

   function Convert (Value : Digit_T)
                     return Digit_Name_T is
     (case Value is when 0 => Zero,  when 1 => One,
                    when 2 => Two,   when 3 => Three,
                    when 4 => Four,  when 5 => Five,
                    when 6 => Six,   when 7 => Seven,
                    when 8 => Eight, when 9 => Nine);

   function Convert (Value : Digit_Name_T) return Digit_T is
     (case Value is when Zero  => 0, when One => 1,
                    when Two   => 2, when Three => 3,
                    when Four  => 4, when Five => 5,
                    when Six   => 6, when Seven => 7,
                    when Eight => 8, when Nine => 9);

   function Convert (Value : Character) return Digit_Name_T is
     (case Value is when '0' => Zero,  when '1' => One,
                    when '2' => Two,   when '3' => Three,
                    when '4' => Four,  when '5' => Five,
                    when '6' => Six,   when '7' => Seven,
                    when '8' => Eight, when '9' => Nine,
                    when others => Zero);

   function Convert (Value : String) return Digit_T is
     (Convert (Digit_Name_T'Value (Value)));

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
               if Converted = Last_Entry
               then
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
               if Converted = Last_Entry
               then
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
