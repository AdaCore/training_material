with Ada.Text_IO; use Ada.Text_IO;
procedure Main is

   --|conversions_begin
   subtype Digit_T is Character range '0' .. '9';
   type Digit_Name_T is
     (Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine);

   function Convert (Value : Digit_T) return Digit_Name_T;
   function Convert (Value : Digit_Name_T) return Digit_T;
   function Convert (Value : Digit_Name_T) return Integer;

   function Convert (Value : Digit_T) return Digit_Name_T is
     (case Value is
         when '0' => Zero,  when '1' => One,   when '2' => Two,
         when '3' => Three, when '4' => Four,  when '5' => Five,
         when '6' => Six,   when '7' => Seven, when '8' => Eight,
         when '9' => Nine);

   function Convert (Value : Digit_Name_T) return Digit_T is
     (case Value is
         when Zero => '0',  when One => '1',   when Two => '2',
         when Three => '3', when Four => '4',  when Five => '5',
         when Six => '6',   when Seven => '7', when Eight => '8',
         when Nine => '9');

   function Convert (Value : Digit_Name_T) return Integer is
     (Digit_Name_T'Pos (Value));
   --|conversions_end

   --|operators_begin
   function "+" (Left  : Digit_T;
                 Right : Digit_Name_T)
                 return Integer;
   function "+" (Left  : Digit_Name_T;
                 Right : Digit_T)
                 return Integer;
   function "+" (Left  : Digit_T;
                 Right : Digit_T)
                 return Integer;
   function "+" (Left  : Digit_Name_T;
                 Right : Digit_Name_T)
                 return Integer;

   function "+" (Left  : Digit_T;
                 Right : Digit_Name_T)
                 return Integer is
      L : constant Digit_Name_T := Convert (Left);
   begin
      return L + Right;
   end "+";

   function "+" (Left  : Digit_Name_T;
                 Right : Digit_T)
                 return Integer is
      Sum : constant Integer := Convert (Left) + Convert (Right);
   begin
      return Sum;
   end "+";

   function "+" (Left  : Digit_T;
                 Right : Digit_T)
                 return Integer is
      L : constant Digit_Name_T := Convert (Left);
      R : constant Digit_Name_T := Convert (Right);
   begin
      return L + R;
   end "+";

   function "+" (Left  : Digit_Name_T;
                 Right : Digit_Name_T)
                 return Integer is
     (Integer'(Convert (Left)) + Integer'(Convert (Right)));
   --|operators_end

--|main_begin
begin

   --  One + 2
   Put_Line (Integer'Image (One + '2'));

   --  3 + Four
   Put_Line (Integer'Image ('3' + Four));

   --  Five + Six
   Put_Line (Integer'Image (Five + Six));

   --  7 + 8
   Put_Line (Integer'Image ('7' + '8'));
end Main;
--|main_end
