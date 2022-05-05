-- Which of the following types is(are) legal?

procedure Main is
   --$ line question
   type T;
   
   --$ line cut
   type Acc is access T;
   --$ line cut
   type Arr is array (1 .. 10) of T;
   --$ line cut
   type T2 is new T;
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
