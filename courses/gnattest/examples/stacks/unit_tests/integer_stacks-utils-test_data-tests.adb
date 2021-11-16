--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Integer_Stacks.Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;
with Integer_Stacks.Stub_Data; use Integer_Stacks.Stub_Data;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Integer_Stacks.Utils.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Reset (Gnattest_T : in out Test);
   procedure Test_Reset_7ce731 (Gnattest_T : in out Test) renames Test_Reset;
--  id:2.2/7ce731a4833f3a32/Reset/1/0/
   procedure Test_Reset (Gnattest_T : in out Test) is
   --  integer_stacks-utils.ads:2:4:Reset
--  end read only

   begin

      Push (Gnattest_T.S, 1);
      Reset (Gnattest_T.S);

      AUnit.Assertions.Assert
        (Gnattest_T.S.Top = 0,
         "S.Top (" & Gnattest_T.S.Top'Img & ") /= 0");

--  begin read only
   end Test_Reset;
--  end read only

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
end Integer_Stacks.Utils.Test_Data.Tests;
