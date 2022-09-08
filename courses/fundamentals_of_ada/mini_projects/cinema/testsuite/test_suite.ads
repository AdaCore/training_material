with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package Test_Suite is
   
   Verbose : Boolean := False;
   
   type Test_Case_T is abstract tagged private;
   procedure Run (TC : Test_Case_T) is abstract;
   --  Test implementation
   
   type Test_Case_Access_T is access all Test_Case_T'Class;
   procedure Register (TC : Test_Case_Access_T; Name : String);
   --  Register the tests to run as part of the default test suite
   
   procedure Run_All;
   -- Run all the registered tests
   
private
   type Test_Case_T is abstract tagged null record;
end Test_Suite;
