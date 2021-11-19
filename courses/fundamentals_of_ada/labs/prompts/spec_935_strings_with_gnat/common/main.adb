with Ada.Text_IO; use Ada.Text_IO;
with Corpuses; use Corpuses;
with Inputs; use Inputs;

procedure Main is
   Corpus : Corpuses.Corpus_T := Corpuses.Shakespeare;
   subtype Values_Index_T is Positive range 1 .. 3;
   subtype Values_Array_T is Input_Value_Array_T (Values_Index_T);

   Values : Values_Array_T;
   Regexp : Values_Regexp_T := Make (Values_Array_T'Length);
begin

   for J in Values_Index_T loop
      declare
         Value : Input_Value_T renames Values (J);
      begin
         Value := Get;
         Put_Line
           ("Present " & Corpuses.Count (Corpus, Value)'Img
           & " times in the corpus");

         Append (Regexp, To_Pattern (Value));
      end;
   end loop;

   Put_Line
     ("Those " & Values_Array_T'Length'Img
     & " values together are present " & Corpuses.Count (Corpus, Regexp)'Img
     & " times in the corpus");

end Main;
