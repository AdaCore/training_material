package body Counter is
   task body Counter_T is
   begin
      accept Initialize
        (Register   : Datastore.Register_T;
         Value      : Integer;
         Increment  : Integer;
         Delay_Time : Duration);
      delay 0.0;
      Datastore.Write
        (Register => Datastore.Register_T'First,
         Value    => 0);
   end Counter_T;
end Counter;
