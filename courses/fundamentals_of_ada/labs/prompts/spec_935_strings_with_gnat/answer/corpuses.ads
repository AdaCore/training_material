with Ada.Strings.Unbounded;
with Inputs;
with Data_Files;

package Corpuses is
   type Corpus_T is private;

   Shakespeare : constant Corpus_T;

   -- Question 2b
   -- Implement this
   function Count (C : Corpus_T; V : Inputs.Input_Value_T)
     return Natural;

   -- Question 3c
   -- Implement this
   function Count (C : Corpus_T; R : Inputs.Values_Regexp_T)
     return Natural;

private

   type Corpus_T is new Ada.Strings.Unbounded.Unbounded_String;

   function Length (C : Corpus_T) return Natural is
      (Ada.Strings.Unbounded.Length (Ada.Strings.Unbounded.Unbounded_String (C)));

   function Slice (C : Corpus_T; From, To : Natural) return String is
      (Ada.Strings.Unbounded.Slice
        (Ada.Strings.Unbounded.Unbounded_String (C),
        From, To));

   Shakespeare : constant Corpus_T := Corpus_T (Data_Files.Read ("romeo_and_juliet.txt"));
end Corpuses;
