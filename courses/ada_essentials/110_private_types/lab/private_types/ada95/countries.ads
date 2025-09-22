with Types; use Types;
with Color_Set;
package Countries is
   subtype Key_T is Types.Countries_T;
   type Map_Component_T is private;
   type Map_T is private;

   procedure Add
     (Map       : in out Map_T;
      Country   :        Key_T;
      Continent :        Continents_T;
      Colors    :        Color_Set.Color_Set_T);

   function Exists
     (Map     : Map_T;
      Country : Key_T)
      return Boolean;
   function Get
     (Map     : Map_T;
      Country : Key_T)
      return Map_Component_T;
   function Is_Valid
     (Component : Map_Component_T)
      return Boolean;
   function Colors
     (Component : Map_Component_T)
      return Color_Set.Color_Set_T;
   function Continent
     (Component : Map_Component_T)
      return Types.Continents_T;
   function Country
     (Component : Map_Component_T)
      return Types.Countries_T;
   function Image
     (Item : Map_Component_T)
      return String;
   function Image
     (Map : Map_T)
      return String;
private
   type Map_Component_T is record
      Valid     : Boolean               := False;
      Country   : Key_T                 := Key_T'First;
      Continent : Continents_T          := Continents_T'First;
      Colors    : Color_Set.Color_Set_T := Color_Set.Empty_Set;
   end record;
   type Map_Array_T is array (1 .. 100) of Map_Component_T;
   type Map_T is record
      Values : Map_Array_T;
      Length : Natural := 0;
   end record;
end Countries;
