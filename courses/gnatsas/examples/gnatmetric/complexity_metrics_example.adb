package body Complexity_Metrics_Example is

   procedure Example (S : in out String) is
      Retval : String (S'First .. S'Last);
      Next   : Integer := S'First;
      procedure Set (C : Character) is
      begin
         Retval (Next) := C;
         Next          := Next + 1;
      end Set;
   begin
      if S'Length > 0 then
         for C of reverse S loop
            Set (C);
         end loop;
      end if;
   end Example;

end Complexity_Metrics_Example;
