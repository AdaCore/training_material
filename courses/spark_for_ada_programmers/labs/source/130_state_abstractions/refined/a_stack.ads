package A_Stack with
   SPARK_Mode,
   Abstract_State => The_Stack,
   Initializes    => The_Stack
is

   Stack_Size : constant := 100;
   subtype Item is Integer range 0 .. 20;

   function Is_Empty return Boolean with
      Global => The_Stack;

   function Is_Full return Boolean with
      Global => The_Stack;

   function Top return Item with
      Pre    => not Is_Empty,
      Global => The_Stack;

   procedure Push (It : in Item) with
      Pre    => not Is_Full,
      Global => (In_Out => The_Stack);

   procedure Pop (It : out Item) with
      Pre    => not Is_Empty,
      Global => (In_Out => The_Stack);

end A_Stack;
