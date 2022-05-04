--  Which of the following piece(s) of code is(are) legal?
procedure Main is
   package Test is
   --$ begin question
   type T1 is range 0 .. 10;
   type T2 is range 0 .. 10;
   type Tag_T1 is tagged null record;
   type Tag_T2 is tagged null record;
   --$ end question

   --$ line cut
   procedure P (A : T1; B : T2) is null;

   --$ line cut
   procedure P (A : T1; B : Tag_T1) is null;

   --$ line cut
   procedure P (A : T1; B : Tag_T1; C : Tag_T1) is null;

   --$ begin cut
   procedure P (A : T1; B : Tag_T1; C : Tag_T2) is null;
   --  Has two controlling type
   --$ end cut
   end Test;
begin
   null;
end Main;
