.. code:: ada
    :class: ada-syntax-only

   package Unconstrained_Array_Types is
   
      type Index_T is range 1 .. 100;
      type Vector_T is array (Index_T range <>) of Character;
      Wrong : Vector_T (0 .. 10); -- run-time error
      Right : Vector_T (11 .. 20);
   
      type Array_Of_Bits_T is array (Natural range <>) of Boolean;
      Bits8  : Array_Of_Bits_T (0 .. 7);
      Bits16 : Array_Of_Bits_T (1 .. 16);
   
      type Days_T is (Sun, Mon, Tues, Wed, Thu, Fri, Sat);
      type Schedule_T is array (Days_T range <>) of Float;
      Schedule : Schedule_T (Mon .. Fri);
   
      Name : String (1 .. 10);
   
      type Roman_Digit is ('I', 'V', 'X', 'L', 'C', 'D', 'M');
      type Roman_Number is array (Natural range <>) of Roman_Digit;
      Orwellian : constant Roman_Number := "MCMLXXXIV";
   
   end Unconstrained_Array_Types;
