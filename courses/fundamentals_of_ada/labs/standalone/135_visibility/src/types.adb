
package body Types is

   function "/"
     (Distance : Feet_T;
      Time     : Seconds_T)
      return Mph_T is (Convert (Distance) / Convert (Time));

   function "/"
     (Distance : Kilometers_T;
      Time     : Minutes_T)
      return Mph_T is (Convert (Distance) / Convert (Time));

   function "/"
     (Distance : Miles_T;
      Time     : Hours_T)
      return Mph_T is (Mph_T (Distance) / Mph_T (Time));

   function Convert
     (Distance : Feet_T)
      return Miles_T is (Miles_T (Distance) / 5_280.0);
   function Convert
     (Distance : Kilometers_T)
      return Miles_T is (Miles_T (Distance) / 1.6);
   function Convert
     (Time : Seconds_T)
      return Hours_T is (Hours_T (Time) / (60.0 * 60.0));
   function Convert
     (Time : Minutes_T)
      return Hours_T is (Hours_T (Time) / 60.0);

end Types;
