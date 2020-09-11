with Ada.Text_IO; use Ada.Text_IO;

package body Important_Dates with
   SPARK_Mode => On
is

   function "="
     (L, R : Date_T)
      return Boolean is
     (L.Year = R.Year and L.Month = R.Month and L.Day = R.Day);

   function ">="
     (L, R : Date_T)
      return Boolean is
      Ret_Val : Boolean := False;
   begin
      if L.Year > R.Year
      then
         Ret_Val := True;
      elsif L.Year = R.Year
      then
         if L.Month > R.Month
         then
            Ret_Val := True;
         elsif L.Month = R.Month
         then
            if L.Day > R.Day
            then
               Ret_Val := True;
            elsif L.Day = R.Day
            then
               Ret_Val := True;
            end if;
         end if;
      end if;
      return Ret_Val;
   end ">=";

   function ">="
     (L, R : Event_T)
      return Boolean is
      Ret_Val : Boolean := False;
   begin
      if L.Date >= R.Date
      then
         Ret_Val := True;
      elsif L.Date = R.Date
      then
         if L.Description > R.Description
         then
            Ret_Val := True;
         elsif L.Description = R.Description
         then
            Ret_Val := True;
         end if;
      end if;
      return Ret_Val;
   end ">=";

   ---------------
   -- Add_Event --
   ---------------

   procedure Add_Event
     (Calendar    : in out Calendar_T;
      Description :        String;
      Date        :        Date_T) is
      Event : Event_T          := (To_Unbounded_String (Description), Date);
      First : constant Index_T := Calendar.List'First;
      Last  : constant Integer := First + Calendar.In_Use - 1;
   begin
      if Calendar.In_Use = 0
      then
         Calendar.List (First) := Event;
      else
         declare
            Added : Boolean := False;
         begin
            for K in First .. Last
            loop
               if Calendar.List (K) >= Event
               then
                  Calendar.List
                    (K + 1 .. Last + 1) := Calendar.List (K .. Last);
                  Calendar.List (K)     := Event;
                  Added                 := True;
                  exit;
               end if;
            end loop;
            if not Added
            then
               Calendar.List (Last + 1) := Event;
            end if;
         end;
      end if;
      Calendar.In_Use := Calendar.In_Use + 1;
   end Add_Event;

   ------------------
   -- Remove_Event --
   ------------------

   procedure Remove_Event
     (Calendar    : in out Calendar_T;
      Description :        String;
      Date        :        Date_T) is
      Event : Event_T          := (To_Unbounded_String (Description), Date);
      First : constant Index_T := Calendar.List'First;
      Last  : constant Integer := First + Calendar.In_Use - 1;
   begin

      for K in First .. Last
      loop
         if Calendar.List (K) = Event
         then
            Calendar.List
              (K .. Last - 1) := Calendar.List
                (K + 1 .. Last);
            Calendar.In_Use := Calendar.In_Use - 1;
            exit;
         end if;
      end loop;

   end Remove_Event;

   ------------------
   -- Print_Events --
   ------------------

   procedure Print_Events
     (Calendar         : Calendar_T;
      Number_Of_Events : Positive;
      Date             : Date_T) is
      First : constant Index_T := Calendar.List'First;
      Last  : constant Integer := First + Calendar.In_Use - 1;

   begin
      Outer_Loop :
      for K in First .. Last
      loop
         if Calendar.List (K).Date >= Date
         then
            for J in K .. Integer'Min (Last, K + Number_Of_Events - 1)
            loop
               Put
                 (Calendar.List (J).Date.Year'Image & "-" &
                  Calendar.List (J).Date.Month'Image & "-" &
                  Calendar.List (J).Date.Day'Image);
               Set_Col (15);
               Put_Line (To_String (Calendar.List (J).Description));
            end loop;
            exit Outer_Loop;

         end if;
      end loop Outer_Loop;
   end Print_Events;

end Important_Dates;
