package body Important_Dates with
   SPARK_Mode => On
is

   ---------------
   -- Add_Event --
   ---------------

   procedure Add_Event
     (Calendar    : in out Calendar_T;
      Description :        String;
      Date        :        Date_T) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Add_Event unimplemented");
   end Add_Event;

   ------------------
   -- Remove_Event --
   ------------------

   procedure Remove_Event
     (Calendar    : in out Calendar_T;
      Description :        String;
      Date        :        Date_T) is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Remove_Event unimplemented");
   end Remove_Event;

   ------------------
   -- Print_Events --
   ------------------

   procedure Print_Events
     (Calendar         : Calendar_T;
      Number_Of_Events : Positive;
      Date             : Date_T) is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Print_Events unimplemented");
   end Print_Events;

end Important_Dates;
