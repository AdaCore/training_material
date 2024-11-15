package body Coupling_Metrics_Dependency is

   function Set
     (A, B : Integer)
      return Record_T is
     ((Field1 => A,
       Field2 => B));
   function Get
     (A : Record_T)
      return Integer is (A.Field1 * A.Field2);
   function Add
     (A, B : Record_T)
      return Record_T is
     ((Field1 => A.Field1 + B.Field1,
       Field2 => A.Field2 + B.Field2));

end Coupling_Metrics_Dependency;
