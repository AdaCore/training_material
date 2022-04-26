with Ada.Calendar;
with Base_Types;
package Sender is

   type Speed_Message_T is record
      Time  : Ada.Calendar.Time;
      Value : Base_Types.Meters_Per_Second_T;
   end record;

   procedure Send (Message : Speed_Message_T);

end Sender;
