with Ada.Text_IO; use Ada.Text_IO;
procedure Main is

   --|conversions_begin
   subtype Digit_T is Character range '0' .. '9';
   type Digit_Name_T is
     (Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine);

   function Convert
     (Value : Digit_T)
      return Digit_Name_T;
   function Convert
     (Value : Digit_Name_T)
      return Digit_T;
   function Convert
     (Value : Digit_Name_T)
      return Integer;

   function Convert
     (Value : Digit_T)
      return Digit_Name_T is
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
      end case;
   end Convert;

   function Convert
     (Value : Digit_Name_T)
      return Digit_T is
   begin
      case Value is
         when Zero =>
            return '0';
         when One =>
            return '1';
         when Two =>
            return '2';
         when Three =>
            return '3';
         when Four =>
            return '4';
         when Five =>
            return '5';
         when Six =>
            return '6';
         when Seven =>
            return '7';
         when Eight =>
            return '8';
         when Nine =>
            return '9';
      end case;
   end Convert;

   function Convert
     (Value : Digit_Name_T)
      return Integer is
   begin
      return Digit_Name_T'Pos (Value);
   end Convert;
   --|conversions_end

   --|operators_begin
   function "+"
     (Left  : Digit_T;
      Right : Digit_Name_T)
      return Integer;
   function "+"
     (Left  : Digit_Name_T;
      Right : Digit_T)
      return Integer;
   function "+"
     (Left  : Digit_T;
      Right : Digit_T)
      return Integer;
   function "+"
     (Left  : Digit_Name_T;
      Right : Digit_Name_T)
      return Integer;

   function "+"
     (Left  : Digit_T;
      Right : Digit_Name_T)
      return Integer is
      L : constant Digit_Name_T := Convert (Left);
   begin
      return L + Right;
   end "+";

   function "+"
     (Left  : Digit_Name_T;
      Right : Digit_T)
      return Integer is
      Sum : constant Integer := Convert (Left) + Convert (Right);
   begin
      return Sum;
   end "+";

   function "+"
     (Left  : Digit_T;
      Right : Digit_T)
      return Integer is
      L : constant Digit_Name_T := Convert (Left);
      R : constant Digit_Name_T := Convert (Right);
   begin
      return L + R;
   end "+";

   function "+"
     (Left  : Digit_Name_T;
      Right : Digit_Name_T)
      return Integer is
   begin
      return Integer'(Convert (Left)) + Integer'(Convert (Right));
   end "+";
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
