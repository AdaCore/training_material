package Types is

   Minimum_Value : constant := 0.0;
   Maximum_Value : constant := 100.0;

   type Numeric_T is digits 6 range Minimum_Value .. Maximum_Value;

   type Record_T is record
      Left     : Numeric_T;
      Right    : Numeric_T;
      Operator : Character;
   end record;

   function Image
     (R : Record_T)
      return String;

end Types;
