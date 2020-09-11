with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Important_Dates with
   SPARK_Mode => On
is

   function Leap_Year
     (Year : Positive)
      return Boolean is
     ((Year mod 4 = 0 and then Year mod 100 /= 0) or else (Year mod 400 = 0));

   function Valid_Date
     (Year  : Positive;
      Month : Positive;
      Day   : Positive)
      return Boolean is
     (case Month is when 4 | 6 | 9 | 11 => Day <= 30,
        when 2      => (if Leap_Year (Year) then Day <= 28 else Day <= 29),
        when others => Day <= 31);

   type Date_T is record
      Year  : Positive := Positive'First;
      Month : Positive := Positive'First;
      Day   : Positive := Positive'First;
   end record with
      Dynamic_Predicate => Valid_Date (Date_T.Year, Date_T.Month, Date_T.Day);
   function "="
     (L, R : Date_T)
      return Boolean;

   type Calendar_T is private;
   function Is_Full
     (Calendar : Calendar_T)
      return Boolean with
      Ghost;

   procedure Add_Event
     (Calendar    : in out Calendar_T;
      Description :        String;
      Date        :        Date_T) with
      Pre => not Is_Full (Calendar);

   procedure Remove_Event
     (Calendar    : in out Calendar_T;
      Description :        String;
      Date        :        Date_T);

   procedure Print_Events
     (Calendar         : Calendar_T;
      Number_Of_Events : Positive;
      Date             : Date_T);

private

   type Event_T is record
      Description : Unbounded_String := To_Unbounded_String ("");
      Date        : Date_T;
   end record;
   function ">="
     (L, R : Event_T)
      return Boolean;

   subtype Index_T is Integer range 1 .. 1_000;
   type Event_List_T is array (Index_T) of Event_T;
   function Is_Sorted
     (List : Event_List_T;
      Used : Integer)
      return Boolean is
     (for all K in List'First .. List'First + Used - 1 =>
        K = List'First or else List (K) >= List (K - 1));

   type Calendar_T is record
      List   : Event_List_T;
      In_Use : Integer := 0;
   end record with
      Type_Invariant => Is_Sorted (Calendar_T.List, Calendar_T.In_Use);

   function Is_Full
     (Calendar : Calendar_T)
      return Boolean is (Calendar.In_Use = Calendar.List'Length);

end Important_Dates;
