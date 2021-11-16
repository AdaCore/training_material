--  This package has been generated automatically by GNATtest.
--  Do not edit any part of it, see GNATtest documentation for more details.

--  begin read only
with Gnattest_Generated;
with AUnit.Test_Caller;

package Integer_Stacks.Test_Data.Tests is

   type Test is new GNATtest_Generated.GNATtest_Standard.Integer_Stacks.Test_Data.Test
   with null record;

   procedure Test_Push_1cad3a (Gnattest_T : in out Test);
   --  integer_stacks.ads:7:4:Push

   procedure Test_Pop_b00d90 (Gnattest_T : in out Test);
   --  integer_stacks.ads:9:4:Pop

   package Caller is new AUnit.Test_Caller (Test);

end Integer_Stacks.Test_Data.Tests;
--  end read only
