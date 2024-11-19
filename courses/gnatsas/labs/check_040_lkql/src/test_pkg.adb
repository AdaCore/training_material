package body Test_Pkg is

   function Supplier1 (X : Good_Candidate) return Good_Candidate is
     (X);

   function Supplier2 (X : Operator_T) return Operator_T is
     (X + 1);

   function Supplier3 (X : Integer) return Conversion_Tgt_T is
     (Conversion_Tgt_T (X));

   function Supplier4 (X : Conversion_Source_T) return Integer is
     (Integer (X));

   function Supplier5 (X : Subtype_T) return Subtype_T is
     (X);

   function Supplier6 (X : Derived_T) return Derived_T is
     (X);

   function Supplier7 (X : Non_Integer_T) return Non_Integer_T is
     (X);

end Test_Pkg;
