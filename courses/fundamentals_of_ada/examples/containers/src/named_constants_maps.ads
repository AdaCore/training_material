with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

package Named_Constants_Maps is
   --$ begin cut
   package Pkg_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => Float,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=");
   --$ end cut
end Named_Constants_Maps;
