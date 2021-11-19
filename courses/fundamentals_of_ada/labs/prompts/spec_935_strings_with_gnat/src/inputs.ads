with Ada.Strings.Bounded;
with GNAT.Regpat;

package Inputs is
   type Input_Value_T is private;
   type Input_Value_Array_T is array (Positive range <>) of Input_Value_T;

   -- Question 1
   -- Implement these
   function To_String (V : Input_Value_T) return String;
   function To_Input_Value (S : String) return Input_Value_T;
   function "&" (S : String; V : Input_Value_T) return String;

   -- Question 2a
   -- Implement this
   function Get return Input_Value_T;

   type Values_Regexp_T (<>) is private;
   function Make (Max_Patterns : Positive) return Values_Regexp_T;
   function To_Pattern_Matcher (R : Values_Regexp_T) return GNAT.Regpat.Pattern_Matcher;

   type Pattern_T is private;

   -- Question 3a
   -- Implement this
   function To_Pattern (S : in Input_Value_T) return Pattern_T;

   -- Question 3b
   -- Implement this
   procedure Append (R : in out Values_Regexp_T; Pattern : Pattern_T);

private

   package Input_Value_Bounded
     is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 100);
   type Input_Value_T is new Input_Value_Bounded.Bounded_String;

   package Pattern_Bounded
     is new Ada.Strings.Bounded.Generic_Bounded_Length
       (Max => 2 * Input_Value_Bounded.Max_Length);
   type Pattern_T is new Pattern_Bounded.Bounded_String;

   type Values_Regexp_T
     (Matcher_Size : GNAT.RegPat.Program_Size; Max_Patterns : Positive) is record
      Matcher : GNAT.RegPat.Pattern_Matcher (Matcher_Size);
      -- TODO add missing components
   end record;

   function To_Pattern_Matcher (R : Values_Regexp_T) return GNAT.Regpat.Pattern_Matcher
      is (R.Matcher);

   function To_String (V : Input_Value_T) return String
     is ("");

   function To_Input_Value (S : String) return Input_Value_T
     is (Input_Value_T (Input_Value_Bounded.Null_Bounded_String));

end Inputs;
