with Test_Suite; use Test_Suite;
with Test_Suite.Test;

package Pixels.Tests is

   procedure Test_Add_Opaque (TC : Test_Case_T'Class);
   
   package Pkg_Test_Add_Opaque is
     new Test_Suite.Test
       (Name => """+"" (opaque)",
        Run_Test  => Test_Add_Opaque);
   
end Pixels.Tests;
