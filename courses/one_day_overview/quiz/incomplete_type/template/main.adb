-- In the same scope, which of the following types is(are) legal?

procedure Main is
   --$ line question
   type T;

   --$ begin cut
   type Acc is access T;
   -- Can :ada:`access` the type
   --$ end cut
   --$ begin cut
   type Arr is array (1 .. 10) of T;
   -- Cannot use the type as a component
   --$ end cut
   --$ begin cut
   type T2 is new T;
   -- Cannot derive from an incomplete type
   --$ end cut
   
   --$ begin cut
   type T2 is record
      Acc : access T;
   end record;
   -- Be careful about the use of an anonymous type here!
   --$ end cut

   type T is new Integer;
begin
   null;
end Main;
