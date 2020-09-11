package Important_Dates with
   SPARK_Mode => On
is

   type Date_T is record
      Year  : Positive := Positive'First;
      Month : Positive := Positive'First;
      Day   : Positive := Positive'First;
   end record;

   type Calendar_T is private;

   procedure Add_Event
     (Calendar    : in out Calendar_T;
      Description :        String;
      Date        :        Date_T);

   procedure Remove_Event
     (Calendar    : in out Calendar_T;
      Description :        String;
      Date        :        Date_T);

   procedure Print_Events
     (Calendar         : Calendar_T;
      Number_Of_Events : Positive;
      Date             : Date_T);

private

   type Calendar_T is null record;

end Important_Dates;
