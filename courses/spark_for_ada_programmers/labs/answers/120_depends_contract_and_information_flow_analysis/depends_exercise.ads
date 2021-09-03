package Depends_Exercise is

   Stack_Size : constant := 100;
   type Pointer_Range is range 0 .. Stack_Size;
   subtype Stack_Range is Pointer_Range range 1 .. Stack_Size;
   Stack : array (Stack_Range) of Integer;
   Stack_Pointer : Pointer_Range := 0;

   -- Add suitable Global and Depends contracts
   procedure Initialize with
     Global => ( input => null,
                 output => (stack, stack_pointer)),
   Depends => (stack_pointer=>null, stack=>null);
      
   -- Add suitable Global and Depends contracts
   procedure Push (X : in Integer) with
     Global => ( input => null,
                 in_out => (stack_pointer, stack)),  
     Depends => (stack_pointer=>stack_pointer, stack=>+(stack_pointer,x));
   
   -- Add suitable Global and Depends contracts
   -- Why might it be useful to put a Depends contract on a function?
   function Is_Full return Boolean with
     Global => ( input => stack_pointer ),
      Depends => (is_full'result=>stack_pointer);


   function Wait_X_Return_True (X : in Integer) return Boolean
     with Depends => (Wait_X_Return_True'Result => null,
                      null => X),
   Global => ( input => null);

end Depends_Exercise;
