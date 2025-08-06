package Protected_Objects is

   protected type Object is
      procedure Set (Caller : Character; V : Integer);
      function Get return Integer;
      procedure Initialize (My_Id : Character);
   
   private
   
      Local : Integer   := 0;
      Id    : Character := ' ';
   end Object;

   O1, O2 : Object;

end Protected_Objects;
