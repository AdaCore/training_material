package Pkg_Vectors is new Ada.Containers.Bounded_Vectors
  (Index_Type => Index_Type, Element_Type => Integer
-- "=" (A, B : Integer) is directly visible
);
