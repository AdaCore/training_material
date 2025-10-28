with Types; use Types;
with Color_Set;
package Countries is

   type Map_Component_T is new Integer;  -- implement this!
   type Map_T is new Integer;            -- implement this!

   --  The country is the "key" for the map
   subtype Key_T is Countries_T;

   procedure Add
     (Map       : in out Map_T;
      Country   :        Key_T;
      Continent :        Continents_T;
      Colors    :        Color_Set.Color_Set_T);

   --  Write query functions to allow you to get information about
   --  a country (continent, flag colors) and convert data to
   --  printable strings.

end Countries;
