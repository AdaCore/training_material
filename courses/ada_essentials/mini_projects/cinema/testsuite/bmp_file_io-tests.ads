with Test_Suite; use Test_Suite;
with Test_Suite.Test;

package BMP_File_IO.Tests is

   procedure Test_Get_Square_10x10 (TC : Test_Case_T'Class);
   
   package Pkg_Test_Get_Square_10x10 is
     new Test_Suite.Test
       (Name => "Get (square 10x10 BMP)",
        Run_Test  => Test_Get_Square_10x10);

end BMP_File_IO.Tests;
