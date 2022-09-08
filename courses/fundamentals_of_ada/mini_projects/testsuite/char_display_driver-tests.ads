with Test_Suite; use Test_Suite;
with Test_Suite.Test;

package Char_Display_Driver.Tests is
   
   procedure Test_Get_Row (TC : Test_Case_T'Class);
   
   package Pkg_Test_Get_Row is
     new Test_Suite.Test
       (Name => "Get_Row (triangle)",
        Run_Test  => Test_Get_Row);

end Char_Display_Driver.Tests;
