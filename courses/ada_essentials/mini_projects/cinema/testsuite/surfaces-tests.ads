with Test_Suite; use Test_Suite;
with Test_Suite.Test;

package Surfaces.Tests is

   procedure Test_Full_White_Row_Luminosity (TC : Test_Case_T'Class);
   
   package Pkg_Test_Full_White_Row_Luminosity is
     new Test_Suite.Test
       (Name => "Get_Row_Luminosity (full white)",
        Run_Test  => Test_Full_White_Row_Luminosity);
   
   procedure Test_Full_Black_Row_Luminosity (TC : Test_Case_T'Class);
   
   package Pkg_Test_Full_Black_Row_Luminosity is
     new Test_Suite.Test
       (Name => "Get_Row_Luminosity (full black)",
        Run_Test  => Test_Full_Black_Row_Luminosity);
   
   procedure Test_Gray_Row_Luminosity (TC : Test_Case_T'Class);
   
   package Pkg_Test_Gray_Row_Luminosity is
     new Test_Suite.Test
       (Name => "Get_Row_Luminosity (gray)",
        Run_Test  => Test_Gray_Row_Luminosity);

end Surfaces.Tests;
