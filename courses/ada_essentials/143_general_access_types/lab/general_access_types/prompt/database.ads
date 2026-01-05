package Database is

   type Dice_T is range 1 .. 6;
   type Attribute_T is array (1 .. 3) of Dice_T;
   type Value_T is
     new Integer range 3 * Integer (Dice_T'First) .. 3 * Integer (Dice_T'Last);

   function Value (Attribute : Attribute_T) return Value_T;
   -- Sum of three dice rolls

   type Reference_T is null record;  -- Implement this!

   function Reference (Value : Value_T) return Reference_T;
   -- Return a pointer to the count for the sum for a particular attribute

   procedure Increment (Attribute : Attribute_T);
   -- Increment the count for this attribute

   procedure Print (Message : String);
   -- Print our table with dice roll sum and number of times it's been hit

end Database;
