with Ada.Exceptions;
with Logs;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;

package body Test_Suite is
   
   function "=" (A, B: Test_Case_Access_T) return Boolean is
   begin
      return A.all = B.all;
   end "=";
   
   type Named_Test_Case_T is record
      TC : Test_Case_Access_T;
      Name : Unbounded_String;
   end record;
      
   package Test_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                       Element_Type => Named_Test_Case_T);
   type Test_Suite_T is tagged record
      Test_Cases : Test_Vectors.Vector;
   end record;
   
   TS : Test_Suite_T;
   procedure Register (TC : Test_Case_Access_T; Name : String) is
   begin
      TS.Test_Cases.Append (Named_Test_Case_T'(TC, To_Unbounded_String (Name)));
   end Register;
   
   procedure Run_All is
      use Logs;
      Successes, Failures, Errors : Natural := 0;
   begin
      if not Verbose then
         Logs.Buffer;
      end if;
      
      for NTC of TS.Test_Cases loop
         Put (" + " & To_String (NTC.Name) & ": ");
         begin
            begin
               NTC.TC.Run;
            exception
               when others =>
                  Logs.Put_Buffer;
                  raise;
            end;
            
            Put_Line (Green, "OK");
            Successes := Successes + 1;
            Logs.Drop_Buffer;
         exception
            when A : Assertion_Error =>
               Put (Red, "Fail ");
               Put_Line (Ada.Exceptions.Exception_Message (A));
               Failures := Failures + 1;
            when E : others =>
               Put (White_On_Red, " Error ");
               Put_Line (" " & Ada.Exceptions.Exception_Name (E));
               Put_Line (Ada.Exceptions.Exception_Message (E));
               Errors := Errors + 1;
         end;
         Logs.Put_Buffer;
      end loop;
      Logs.Unbuffer;
      
      Put_Line ("success:" & Successes'Image & " failures:" & Failures'Image & " errors:" & Errors'Image); 
      if Failures = 0 then
         Put_Line (Green, "OK all tests passed");
      else
         Put_Line (Red, "FAIL");
      end if;
   end Run_All;

end Test_Suite;
