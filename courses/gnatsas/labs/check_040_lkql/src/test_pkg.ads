with Ada.Text_IO;
package Test_Pkg is

   type Good_Candidate is range 0 .. 100;
   function Supplier1 (X : Good_Candidate) return Good_Candidate;

   type Operator_T is range 0 .. 100;
   function Supplier2 (X : Operator_T) return Operator_T;

   type Conversion_Tgt_T is range 0 .. 100;
   function Supplier3 (X : Integer) return Conversion_Tgt_T;

   type Conversion_Source_T is range 0 .. 100;
   function Supplier4 (X : Conversion_Source_T) return Integer;

   type Subtype_Parent_T is range 0 .. 100;
   subtype Subtype_T is Subtype_Parent_T range 1 .. Subtype_Parent_T'Last;
   function Supplier5 (X : Subtype_T) return Subtype_T;

   type Derived_Parent_T is range 0 .. 100;
   type Derived_T is new Derived_Parent_T range 1 .. Derived_Parent_T'Last;
   function Supplier6 (X : Derived_T) return Derived_T;

   type Generic_Instantiaion_T is range 0 .. 100;
   package IO is new Ada.Text_IO.Integer_IO (Generic_Instantiaion_T);

end Test_Pkg;
