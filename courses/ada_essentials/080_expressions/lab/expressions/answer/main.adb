with Ada.Text_IO; use Ada.Text_IO;

procedure Main is

--|checks_begin
   subtype Year_T is Positive range 1_900 .. 2_099;
   subtype Month_T is Positive range 1 .. 12;
   subtype Day_T is Positive range 1 .. 31;

   type Date_T is record
      Year  : Positive;
      Month : Positive;
      Day   : Positive;
   end record;

   type Dates_T is array (1 .. 3) of Date_T;

   function Is_Leap_Year (Year : Positive) return Boolean
   is (Year mod 400 = 0 or else (Year mod 4 = 0 and Year mod 100 /= 0));

   function Days_In_Month (Month : Positive; Year : Positive) return Day_T
   is (case Month is
         when 4 | 6 | 9 | 11 => 30,
         when 2 => (if Is_Leap_Year (Year) then 29 else 28),
         when others => 31);

   function Is_Valid (Date : Date_T) return Boolean
   is (Date.Year in Year_T
       and then Date.Month in Month_T
       and then Date.Day <= Days_In_Month (Date.Month, Date.Year));

   function Any_Invalid (List : Dates_T) return Boolean
   is (for some Date of List => not Is_Valid (Date));

   function Same_Year (List : Dates_T) return Boolean
   is (for all I in List'Range => List (I).Year = List (List'First).Year);
--|checks_end

--|main_begin
   Good_Dates : constant Dates_T :=
     ((Year => 2_025, Month => 1, Day => 2),
      (Year => 2_024, Month => 2, Day => 28),
      (Year => 2_000, Month => 2, Day => 29));

   Mixed_Dates : constant Dates_T :=
     ((Year => 2_025, Month => 4, Day => 30),
      (Year => 2_024, Month => 2, Day => 28),
      (Year => 1_900, Month => 2, Day => 29));

   Same_Year_Dates : constant Dates_T :=
     ((Year => 2_025, Month => 4, Day => 30),
      (Year => 2_025, Month => 2, Day => 28),
      (Year => 2_025, Month => 2, Day => 29));

begin

   Put_Line ("Good_Dates");
   Put_Line ("  Any invalid: " & Boolean'Image (Any_Invalid (Good_Dates)));
   Put_Line ("  Same Year: " & Boolean'Image (Same_Year (Good_Dates)));

   Put_Line ("Mixed_Dates");
   Put_Line ("  Any invalid: " & Boolean'Image (Any_Invalid (Mixed_Dates)));
   Put_Line ("  Same Year: " & Boolean'Image (Same_Year (Mixed_Dates)));

   Put_Line ("Same_Year_Dates");
   Put_Line
     ("  Any invalid: " & Boolean'Image (Any_Invalid (Same_Year_Dates)));
   Put_Line ("  Same Year: " & Boolean'Image (Same_Year (Same_Year_Dates)));
--|main_end

end Main;
