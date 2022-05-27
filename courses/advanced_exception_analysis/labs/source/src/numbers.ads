package Numbers is

   type Quantity_T is range 0 .. 100;
   type Cost_T is digits 6 range 0.0 .. 100.0;

   function Convert
     (Number : Quantity_T)
      return String;
   function Convert
     (Number : String)
      return Quantity_T;

   function Convert
     (Number : Cost_T)
      return String;
   function Convert
     (Number : String)
      return Cost_T;

end Numbers;
