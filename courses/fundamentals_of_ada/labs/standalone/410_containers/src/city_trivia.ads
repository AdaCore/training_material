with Ada.Containers.Bounded_Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package City_Trivia is

  package Strings_Vector is new Ada.Containers.Bounded_Vectors
   (Index_Type => Natural, Element_Type => Unbounded_String);
  subtype Strings_Vector_T is Strings_Vector.Vector (100);

  procedure Add_Trivia
   (City        : String;
    Information : String);

  function Get_Trivia
   (City : String)
    return Strings_Vector_T;
  function Get_Keys return Strings_Vector_T;

  package Sort is new Strings_Vector.Generic_Sorting;

end City_Trivia;
