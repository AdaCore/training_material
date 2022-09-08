with Test_Suite; use Test_Suite;
with Test_Suite.Test;

package Drawable_Chars.Tests is

   procedure Test_By_Black_Ratio (TC : Test_Case_T'Class);
   
   package Pkg_Test_By_Black_Ratio is
     new Test_Suite.Test
       (Name => "Sort (by black ratio)",
        Run_Test  => Test_By_Black_Ratio);
   
   procedure Test_Identical_Values (TC : Test_Case_T'Class);
   
   package Pkg_Test_Identical_Values is
     new Test_Suite.Test
       (Name => "Sort (identical values)",
        Run_Test  => Test_Identical_Values);
   
   procedure Test_Closest_By_Height (TC : Test_Case_T'Class);
   
   package Pkg_Test_Closest_By_Height is
     new Test_Suite.Test
       (Name => "Closest (by height)",
        Run_Test  => Test_Closest_By_Height);
   
end Drawable_Chars.Tests;
