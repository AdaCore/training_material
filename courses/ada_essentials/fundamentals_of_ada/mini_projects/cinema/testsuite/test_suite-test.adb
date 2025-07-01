package body Test_Suite.Test is

   type TC_T is new Test_Case_T with null record;
   
   procedure Run (TC : TC_T) is
   begin
      Run_Test (TC);
   end Run;
   
begin
   Register (new TC_T, Name);
end Test_Suite.Test;
