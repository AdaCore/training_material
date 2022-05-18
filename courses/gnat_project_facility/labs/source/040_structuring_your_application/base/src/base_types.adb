with System;
package body Base_Types is

   type Work_T is digits System.Max_Digits;

   function Speed
     (Meters : Meters_T;
      Time   : Time_T)
      return Meters_Per_Second_T is
      M : Work_T := Work_T (Meters);
      T : Work_T := Work_T (Time);
      R : Work_T;
   begin
      R := M / T;
      return Meters_Per_Second_T (R);
   end Speed;

end Base_Types;
