.. code:: ada

   procedure Visibility_Issues is
      procedure Foo (I : Integer) is
         procedure Foo (N : Natural) is null;
      begin
         Foo (I);
      end Foo;
      -- procedure Foo (N : Natural) is null; -- compile error
   begin
      Foo (1);
   end Visibility_Issues;
