package Protected_Objects is
   
   --$ begin cut
   protected Object
      with Lock_Free is
   --$ end cut

      procedure Set (V : Integer);
      function Get return Integer;

   private
      Local : Integer := 0;
   end Object;

end Protected_Objects;
