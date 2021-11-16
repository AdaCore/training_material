--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Simple.Test_Data.

--$ begin cut
with AUnit.Assertions; use AUnit.Assertions;
--$ end cut
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
--$ begin cut
package body Simple.Test_Data.Tests is
--$ end cut

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--$ begin cut
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
        (Inc (1) = 2,
         "Inc (1) = " & Inc (1)'Img & " /= 2");

--  begin read only
   end Test_Inc;
--  end read only
--$ end cut

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
--$ begin cut
end Simple.Test_Data.Tests;
--$ end cut
