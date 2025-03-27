package Coupling_Metrics_Dependency is

   type Record_T is tagged private;
   function Set
     (A, B : Integer)
      return Record_T;
   function Get
     (A : Record_T)
      return Integer;
   function Add
     (A, B : Record_T)
      return Record_T;

private
   type Record_T is tagged record
      Component1, Component2 : Integer;
   end record;

end Coupling_Metrics_Dependency;
