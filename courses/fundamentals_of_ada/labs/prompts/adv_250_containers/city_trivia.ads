-- "with" appropriate container(s)
package City_Trivia is

   procedure Add_Trivia
     (City        : String;
      Information : String);

   function Get_Trivia
     (City : String)
      return String; -- actually need a collection of strings

   function Get_Cities return String; -- actually need a collection of strings

end City_Trivia;
