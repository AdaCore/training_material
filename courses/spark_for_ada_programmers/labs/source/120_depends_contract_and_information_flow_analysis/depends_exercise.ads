package Depends_Exercise is

   Stack_Size : constant := 100;
   type Pointer_Range is range 0 .. Stack_Size;
   subtype Stack_Range is Pointer_Range range 1 .. Stack_Size;
   Stack : array (Stack_Range) of Integer;
   Stack_Pointer : Pointer_Range := 0;

   -- Add suitable Global and Depends contracts
   procedure Initialize;
      
   -- Add suitable Global and Depends contracts
   procedure Push (X : in Integer);
   
   -- Add suitable Global and Depends contracts
   -- Why might it be useful to put a Depends contract on a function?
   function Is_Full return Boolean;

   function Wait_X_Return_True (X : in Integer) return Boolean;

end Depends_Exercise;
