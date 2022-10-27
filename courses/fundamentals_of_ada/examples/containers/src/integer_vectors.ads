with Ada.Containers.Bounded_Vectors;

generic
   type Index_Type is range <>;
package Integer_Vectors is

   --$ begin cut
   package Pkg_Vectors is new Ada.Containers.Bounded_Vectors
     (Index_Type => Index_Type, Element_Type => Integer
   -- "=" (A, B : Integer) is directly visible
   );
   --$ end cut

   procedure Sort
     (V    : in out Pkg_Vectors.Vector; First : Index_Type;
      Last :        Index_Type);

end Integer_Vectors;
