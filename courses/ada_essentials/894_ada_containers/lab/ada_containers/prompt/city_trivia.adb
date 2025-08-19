-- with appropriate container to build a map
package body City_Trivia is

   Map : Integer;  -- actually needs to be a map

   procedure Add_Trivia
     (City        : String;
      Information : String) is null;

   function Get_Trivia
     (City : String)
      return String is ("");

   function Get_Cities return String is ("");

end City_Trivia;
