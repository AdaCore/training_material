with Ada.Text_IO; use Ada.Text_IO;

procedure Main is

   subtype Year_T is Positive range 1_900 .. 2_099;
   subtype Month_T is Positive range 1 .. 12;
   subtype Day_T is Positive range 1 .. 31;

   type Date_T is record
      Year  : Positive;
      Month : Positive;
      Day   : Positive;
   end record;

   type Dates_T is array (1 .. 3) of Date_T;
   
   -- implement correctly as an expression function
   function Is_Leap_Year (Year : Positive) return Boolean is (False);

   function Days_In_Month (Month : Positive; Year  : Positive) return Day_T is (1);

   function Is_Valid (Date : Date_T) return Boolean is (False);

   function Any_Invalid (List : Dates_T) return Boolean is (False);

   function Same_Year (List : Dates_T) return Boolean is (False);

   Valid_Dates : constant Dates_T :=
     ((Year => 2_025, Month => 1, Day => 2),
      (Year => 2_024, Month => 2, Day => 28),
      (Year => 2_000, Month => 2, Day => 29));

   Valid_And_Invalid_Dates : constant Dates_T :=
     ((Year => 2_025, Month => 4, Day => 30),
      (Year => 2_024, Month => 2, Day => 28),
      (Year => 1_900, Month => 2, Day => 29));

   Same_Year_Dates : constant Dates_T :=
     ((Year => 2_025, Month => 4, Day => 30),
      (Year => 2_025, Month => 2, Day => 28),
      (Year => 2_025, Month => 2, Day => 29));

begin

   Put_Line ("Valid_Dates");
   Put_Line ("  Any invalid: " & Boolean'Image (Any_Invalid (Valid_Dates)));
   Put_Line ("  Same Year: " & Boolean'Image (Same_Year (Valid_Dates)));

   Put_Line ("Valid_And_Invalid_Dates");
   Put_Line ("  Any invalid: " & Boolean'Image (Any_Invalid (Valid_And_Invalid_Dates)));
   Put_Line ("  Same Year: " & Boolean'Image (Same_Year (Valid_And_Invalid_Dates)));

   Put_Line ("Same_Year_Dates");
   Put_Line ("  Any invalid: " & Boolean'Image (Any_Invalid (Same_Year_Dates)));
   Put_Line ("  Same Year: " & Boolean'Image (Same_Year (Same_Year_Dates)));

end Main;
