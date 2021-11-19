with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Regpat;

package body Corpuses is

   function Count (C : Corpus_T; V : Inputs.Input_Value_T) return Natural is
   begin
      return 0; --  TODO
   end Count;

   function Count (C : Corpus_T; R : Inputs.Values_Regexp_T) return Natural is
   begin
      return 0; --  TODO
   end Count;

end Corpuses;
