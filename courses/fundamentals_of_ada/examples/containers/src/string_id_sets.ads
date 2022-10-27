with Ada.Containers.Indefinite_Ordered_Sets;

package String_Id_Sets is

   --$ begin cut
   package Pkg_Sets is new Ada.Containers
     .Indefinite_Ordered_Sets
     (Element_Type => String);
   --$ end cut

end String_Id_Sets;
