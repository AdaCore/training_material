package A_Stack with
   SPARK_Mode
is

   Stack_Size : constant := 100;
   subtype Item is Integer range 0 .. 20;

   subtype Stack_Pointer is Natural range 0 .. Stack_Size;
   subtype Index is Stack_Pointer range 1 .. Stack_Size;
   type Vector is array (Index) of Item;

   Sp  : Stack_Pointer := 0;
   Vec : Vector        := (others => 0);

   function Is_Empty return Boolean with
      Global => Sp;

   function Is_Full return Boolean with
      Global => Sp;

   function Top return Item with
      Global => (Sp, Vec);

   procedure Push (It : in Item) with
      Global => (In_Out => (Sp, Vec));

   procedure Pop (It : out Item) with
      Global => (Input => Vec, In_Out => Sp);

end A_Stack;
