with AUnit.Assertions; use AUnit.Assertions;
...
package body Simple.Test_Data.Tests is
...
--  begin read only
   procedure Test_Inc (Gnattest_T : in out Test);
   procedure Test_Inc_4f8b9f (Gnattest_T : in out Test) renames Test_Inc;
--  id:2.2/4f8b9f38b0ce8c74/Inc/1/0/
   procedure Test_Inc (Gnattest_T : in out Test) is
   --  simple.ads:8:4:Inc
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Inc;
--  end read only
...
end Simple.Test_Data.Tests;
