generic
   type Element_Type is private;

   type List_Type  is array (Integer range <>) of Element_Type;

   with function Compare (Left : Element_Type; Right : Element_Type) return Boolean;

   with function To_String (E : Element_Type) return String;

package Sort_Generics is

   procedure Sort_Generic (List : in out List_Type);

   procedure Display_List (List : in List_Type);

end Sort_Generics;
