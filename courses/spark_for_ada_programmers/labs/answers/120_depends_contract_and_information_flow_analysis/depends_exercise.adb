package body Depends_Exercise is

   procedure Initialize is
   begin
      Stack_Pointer := 0;
      Stack := (others => 0);
   end Initialize;

   procedure Push (X : in Integer) is
   begin
      Stack_Pointer := Stack_Pointer + 1;
      Stack (Stack_Pointer) := X;
   end Push;

   function Is_Full return Boolean is
   begin
      return Stack_Pointer = Stack_Size;
   end Is_Full;

   function Wait_X_Return_True (X : in Integer) return Boolean is
   begin
      for I in Integer range 1 .. X loop
        null;
      end loop;
      return True;
   end Wait_X_return_True;

end Depends_Exercise;
