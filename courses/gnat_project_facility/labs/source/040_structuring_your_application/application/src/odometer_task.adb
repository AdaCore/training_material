with Ada.Calendar; use Ada.Calendar;
with Base_Types;
with Odometer;
with Sender;
package body Odometer_Task is

   use type Base_Types.Meters_T;

   Last_Reading : Base_Types.Meters_T;
   Last_Clock   : Ada.Calendar.Time;

   task body Monitor is
   begin
      accept Initialize do
         Last_Reading := Odometer.Read;
         Last_Clock   := Ada.Calendar.Clock;
      end Initialize;
      loop
         select
            accept Finalize;
            exit;
         else
            delay 0.5;
            declare
               Reading : Base_Types.Meters_T            := Odometer.Read;
               Clock   : Ada.Calendar.Time              := Ada.Calendar.Clock;
               Speed   : Base_Types.Meters_Per_Second_T :=
                 Base_Types.Speed
                   (Reading - Last_Reading,
                    Base_Types.Time_T (Clock - Last_Clock));
            begin
               Sender.Send
                 (Message =>
                    (Clock,
                     Speed));
               Last_Reading := Reading;
               Last_Clock   := Clock;
            end;
         end select;
      end loop;
   end Monitor;

end Odometer_Task;
