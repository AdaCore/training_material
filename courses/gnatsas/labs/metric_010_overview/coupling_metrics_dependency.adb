package body Coupling_Metrics_Dependency is

   function Set
     (A, B : Integer)
      return Record_T is
     ((Component1 => A,
       Component2 => B));
   function Get
     (A : Record_T)
      return Integer is (A.Component1 * A.Component2);
   function Add
     (A, B : Record_T)
      return Record_T is
     ((Component1 => A.Component1 + B.Component1,
       Component2 => A.Component2 + B.Component2));

end Coupling_Metrics_Dependency;
