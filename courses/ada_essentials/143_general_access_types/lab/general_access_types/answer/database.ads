package Database is

   type Dice_T is range 1 .. 6;
   type Attribute_T is array (1 .. 3) of Dice_T;
   type Value_T is
     new Integer range 3 * Integer (Dice_T'First) .. 3 * Integer (Dice_T'Last);

   function Value (Attribute : Attribute_T) return Value_T;

   type Reference_T is access all Natural;
   function Reference (Value : Value_T) return Reference_T;
   procedure Increment (Attribute : Attribute_T);
   procedure Print (Message : String);

end Database;
