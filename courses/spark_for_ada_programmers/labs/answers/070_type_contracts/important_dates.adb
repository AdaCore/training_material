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
      return Boolean
   is
      (L.Year > R.Year
       or else
         (L.Year = R.Year
          and then
            (L.Month > R.Month
             or else (L.Month = R.Month
               and then L.Day >= R.Day))));

   function ">="
     (L, R : Event_T)
      return Boolean
   is
      (L.Date >= R.Date);

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
                  pragma Assert (K = First or else not (Calendar.List (K - 1) >= Event));
                  pragma Assert (K = First or else Event >= Calendar.List (K - 1));
                  pragma Assert (Calendar.List (K + 1) >= Event);
                  Calendar.List (K)     := Event;
                  Added                 := True;
                  pragma Assert
                    (for all J in First .. Last + 1 =>
                       J = First or else Calendar.List (J) >= Calendar.List (J - 1));
                  exit;
               end if;
               pragma Loop_Invariant
                 (for all J in First .. K => not (Calendar.List (J) >= Event));
            end loop;
            if not Added
            then
               pragma Assert (Event >= Calendar.List (Last));
               Calendar.List (Last + 1) := Event;
               pragma Assert
                 (for all J in First .. Last + 1 =>
                    J = First or else Calendar.List (J) >= Calendar.List (J - 1));
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
            pragma Assert
              (K = First
               or else K = Last
               or else Calendar.List (K + 1) >= Calendar.List (K - 1));
            Calendar.List (K .. Last - 1) := Calendar.List (K + 1 .. Last);
            pragma Assert
              (for all J in First .. Last - 1 =>
                 J = First or else Calendar.List (J) >= Calendar.List (J - 1));
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
            for J in K .. Last
            loop
               exit when J - K + 1 > Number_Of_Events;
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
