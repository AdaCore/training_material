with Ada.Containers.Vectors;

package City_Trivia is

   subtype City_Name_T is String (1 .. 10);
   subtype Information_T is String (1 .. 30);

   package City_List is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => City_Name_T);

   package Information_List is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => Information_T);

   procedure Add_Trivia (City : String; Information : String);

   function Get_Trivia (City : String) return Information_List.Vector;

   function Get_Cities return City_List.Vector;

   package City_Sort is new City_List.Generic_Sorting;
   package Information_Sort is new Information_List.Generic_Sorting;

end City_Trivia;
