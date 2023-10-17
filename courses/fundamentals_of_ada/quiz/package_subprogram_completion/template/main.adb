-- Which completion(s) is(are) correct for :ada:`package P`?
procedure Main is
   --$ begin question
   package P is
      Object_One : Integer;
      procedure One (P : out Integer);
   end P;
   --$ end question
   
   --$ begin cut
   No completion is needed
   -- Procedure One must have a body
   --$ end cut
   
   --$ begin cut
   package body P is
      procedure One (P : out Integer) is null;
   end P;
   -- Parameter P is :ada:`out` but not assigned (legal but not a good idea)
   --$ end cut

   --$ begin cut
   package body P is
      Object_One : Integer;
      procedure One (P : out Integer) is
      begin
         P := Object_One;
      end One;
   end P;
   -- Redeclaration of Object_One
   --$ end cut

   --$ begin cut
   package body P is
      procedure One (P : out Integer) is
      begin
         P := Object_One;
      end One;
   end P;
   -- Correct
   --$ end cut

begin
   null;
end Main;
