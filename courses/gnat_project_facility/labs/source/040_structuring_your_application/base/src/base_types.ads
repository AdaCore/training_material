with Ada.Calendar;
package Base_Types is

   type Meters_T is digits 6;
   type Time_T is new Ada.Calendar.Day_Duration;
   type Meters_Per_Second_T is digits 6;

   function Speed
     (Meters : Meters_T;
      Time   : Time_T)
      return Meters_Per_Second_T;

end Base_Types;
