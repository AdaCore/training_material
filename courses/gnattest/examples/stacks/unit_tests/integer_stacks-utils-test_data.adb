--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body Integer_Stacks.Utils.Test_Data is

   procedure Set_Up (Gnattest_T : in out Test) is
      New_S : Stack;
   begin
      Gnattest_T.S := New_S;
   end Set_Up;

   procedure Tear_Down (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      null;
   end Tear_Down;end Integer_Stacks.Utils.Test_Data;
