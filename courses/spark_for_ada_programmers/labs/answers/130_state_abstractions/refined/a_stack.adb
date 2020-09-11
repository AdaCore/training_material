package body A_Stack with
   SPARK_Mode,
   --  The_Stack is actually refined into two constituents
   Refined_State => (The_Stack => (P, V, M))
is

   subtype Pointer_T is Integer range 0 .. Stack_Size;
   subtype Index_T is Pointer_T range 1 .. Pointer_T'Last;
   P : Pointer_T               := 0;
   V : array (Index_T) of Item := (others => Item'First);
   M : Integer                 := 0;

   function Is_Empty return Boolean is (P = 0) with
      Refined_Global => P;
   function Is_Full return Boolean is (P = Stack_Size) with
      Refined_Global => P;
   function Top return Item is (V (P)) with
      Refined_Global => (P, V);

      ----------
      -- Push --
      ----------

   procedure Push (It : in Item) with
      Refined_Global => (In_Out => (P, V, M))
   is
   begin
      P     := P + 1;
      V (P) := It;
      M     := Integer'Max (P, M);
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (It : out Item) with
      Refined_Global => (In_Out => P, Input => V)
   is
   begin
      It := V (P);
      P  := P - 1;
   end Pop;

   function Utilization return Integer is (M) with
      Refined_Global => M;

end A_Stack;
