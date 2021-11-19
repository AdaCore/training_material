with Ada.Strings.Unbounded;
with Ada.Text_IO.Bounded_IO;

package body Inputs is

   package Bounded_Text_IO
     is new Ada.Text_IO.Bounded_IO (Input_Value_Bounded);

   function "&" (S : String; V : Input_Value_T) return String is
   begin
      return S & To_String (V);
   end "&";

   -- Question 2 Tip
   -- You will need to use the with'ed Text_IO.Bounded_IO
   function Get return Input_Value_T is
   begin
      return +Bounded_Text_IO.Get_Line;
   end Get;

   function To_Pattern (S : Input_Value_T) return Pattern_T is
   begin
      return +Pattern_Bounded.To_Bounded_String
        (GNAT.Regpat.Quote (To_String (S)));
   end To_Pattern;

   procedure Append (R : in out Values_Regexp_T; Pattern : Pattern_T) is
      use Ada.Strings.Unbounded;
   begin
      R.Patterns_Count := R.Patterns_Count + 1;
      R.Patterns (R.Patterns_Count) := Pattern;
      declare
         Pat_Total : Unbounded_String;
      begin
         for J in 1 .. R.Patterns_Count loop
            if Length (R.Patterns (J)) /= 0 then
               if Length (Pat_Total) /= 0 then
                   Append (Pat_Total, "|");
               end if;
               Append (Pat_Total, To_String (R.Patterns (J)));
            end if;
         end loop;

         GNAT.Regpat.Compile (R.Matcher, To_String (Pat_Total));
      end;
   end Append;

   function Make (Max_Patterns : Positive) return Values_Regexp_T is
      V : Values_Regexp_T
        (GNAT.RegPat.Program_Size (Max_Patterns * Pattern_Bounded.Max_Length),
         Max_Patterns => Max_Patterns);
   begin
      V.Patterns_Count := 0;
      return V;
   end Make;

end Inputs;
