with Ada.Text_IO.Bounded_IO;

package body Inputs is


   function "&" (S : String; V : Input_Value_T) return String is
   begin
      return S & "<Input>"; -- TODO
   end "&";

   -- Question 2 Tip
   -- You will need to use the with'ed Text_IO.Bounded_IO
   function Get return Input_Value_T is
   begin
      return I : Input_Value_T; -- TODO
   end Get;

   function To_Pattern (S : Input_Value_T) return Pattern_T is
   begin
      return T : Pattern_T; -- TODO
   end To_Pattern;

   procedure Append (R : in out Values_Regexp_T; Pattern : Pattern_T) is
   begin
      null; -- TODO
   end Append;

   function Make (Max_Patterns : Positive) return Values_Regexp_T is
      V : Values_Regexp_T
        (GNAT.RegPat.Program_Size (Max_Patterns * Pattern_Bounded.Max_Length),
         Max_Patterns => Max_Patterns);
   begin
      return V;
   end Make;

end Inputs;
