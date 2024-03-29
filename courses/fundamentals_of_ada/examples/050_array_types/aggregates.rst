.. code:: ada

   procedure Aggregates is
   
      type Days_T is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
      type Working_T is array (Days_T) of Float;
      Week : Working_T := (others => 0.0);
   
      Start, Finish : Days_T;
   
      type Array_T is array (Days_T range <>) of Boolean;
      Vector : Array_T (Mon .. Start) := (others => False);
   
   begin
   
      Week := (8.0, 8.0, 8.0, 8.0, 8.0, 0.0, 0.0);
      Week := (Sat => 0.0, Sun => 0.0, Mon .. Fri => 8.0);
      Week := (Sat | Sun => 0.0, Mon .. Fri => 8.0);
      -- Compile error
      -- Week := (8.0, 8.0, 8.0, 8.0, 8.0, Sat => 0.0, Sun => 0.0);
   
      if Week = (10.0, 10.0, 10.0, 10.0, 0.0, 0.0, 0.0) then
         null; -- four-day week
      end if;
   
      Week := (8.0, others => 0.0);
      Week := (8.0, others => <>); -- Ada2012: use previously set values
   
      -- Compile error
      -- Week := (Week'First .. Start => 0.0, Start .. Finish => 8.0,
      --          Finish .. Week'Last => 0.0);
   
   end Aggregates;
