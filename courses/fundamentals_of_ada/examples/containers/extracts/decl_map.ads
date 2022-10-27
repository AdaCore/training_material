package Pkg_Maps is new Ada.Containers.Hashed_Maps
  (Key_Type => Ada.Strings.Unbounded.Unbounded_String,
   Element_Type    => Float,
   Hash            => Ada.Strings.Unbounded.Hash,
   Equivalent_Keys => Ada.Strings.Unbounded."=");
