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

   -- implement correctly as an expression function
   function Is_Leap_Year
     (Year : Positive)
      return Boolean is (False);

   -- implement correctly as an expression function
   -- (case expression would be helpful)
   function Days_In_Month
     (Month : Positive;
      Year  : Positive)
      return Positive is (1);

   -- Implement correctly as an expression function
   function Is_Valid
     (Date : Date_T)
      return Boolean is (False);

   List : array (1 .. 5) of Date_T;
   Item : Date_T;

   function Number
     (Prompt : String)
      return Positive is
   begin
      Put (Prompt & "> ");
      return Positive'value (Get_Line);
   end Number;

   function Any_Invalid return Boolean is (False);
   function Same_Year return Boolean is (False);

begin

   for I in List'range loop
      Item.Year  := Number ("Year");
      Item.Month := Number ("Month");
      Item.Day   := Number ("Day");
      List (I)   := Item;
   end loop;

   -- Print True/False if any date in the list is not valid
   Put_Line ("Any invalid: " & Boolean'image (Any_Invalid));
   -- Print True/False if all dates in the list are in the same year
   Put_Line ("Same Year: " & Boolean'image (Same_Year));

end Main;
