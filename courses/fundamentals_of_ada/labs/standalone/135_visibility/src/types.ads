package Types is

   type Mph_T is digits 6;

   type Feet_T is digits 6;
   type Miles_T is digits 6;
   type Kilometers_T is digits 6;

   type Seconds_T is digits 6;
   type Minutes_T is digits 6;
   type Hours_T is digits 6;

   function "/"
     (Distance : Feet_T;
      Time     : Seconds_T)
      return Mph_T;

   function "/"
     (Distance : Kilometers_T;
      Time     : Minutes_T)
      return Mph_T;

   function "/"
     (Distance : Miles_T;
      Time     : Hours_T)
      return Mph_T;

   function Convert
     (Distance : Feet_T)
      return Miles_T;
   function Convert
     (Distance : Kilometers_T)
      return Miles_T;
   function Convert
     (Time : Seconds_T)
      return Hours_T;
   function Convert
     (Time : Minutes_T)
      return Hours_T;

end Types;
