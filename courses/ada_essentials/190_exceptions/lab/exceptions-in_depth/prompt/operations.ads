package Operations is

   --  Define your exceptions here

   Maximum_Value : constant := 100.0;
   type Numeric_T is digits 6 range -Maximum_Value .. Maximum_Value;
   type Operator_T is ('+', '-', '*', '/');

   type Operation_T is record
      Left     : Float;
      Operator : Operator_T;
      Right    : Float;
   end record;

   function Perform (Operation : Operation_T) return Numeric_T;

end Operations;
