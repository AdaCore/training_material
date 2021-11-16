--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Integer_Stacks.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Integer_Stacks.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Push (Gnattest_T : in out Test);
   procedure Test_Push_1cad3a (Gnattest_T : in out Test) renames Test_Push;
--  id:2.2/1cad3af5b932b427/Push/1/0/
   procedure Test_Push (Gnattest_T : in out Test) is
   --  integer_stacks.ads:7:4:Push
--  end read only

   begin

      Push (Gnattest_T.S, 1);

      AUnit.Assertions.Assert
        (Gnattest_T.S.Top = 1,
         "S.Top (" & Gnattest_T.S.Top'Img & ") /= 1");

--  begin read only
   end Test_Push;
--  end read only


--  begin read only
   procedure Test_Pop (Gnattest_T : in out Test);
   procedure Test_Pop_b00d90 (Gnattest_T : in out Test) renames Test_Pop;
--  id:2.2/b00d90e7b603534f/Pop/1/0/
   procedure Test_Pop (Gnattest_T : in out Test) is
   --  integer_stacks.ads:9:4:Pop
--  end read only
    
      I : Integer;
   begin

      Push (Gnattest_T.S, 10);
      Pop (Gnattest_T.S, I);

      AUnit.Assertions.Assert
        (I = 10,
         "I (" & I'Img & ") /= 10");

      AUnit.Assertions.Assert
        (Gnattest_T.S.Top = 0,
         "S.Top (" & Gnattest_T.S.Top'Img & ") /= 0");

--  begin read only
   end Test_Pop;
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
end Integer_Stacks.Test_Data.Tests;
