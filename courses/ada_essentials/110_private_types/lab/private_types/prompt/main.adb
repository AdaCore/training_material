with Ada.Text_IO; use Ada.Text_IO;
with Color_Set;
with Countries;
with Types;       use Types;
procedure Main is
begin

   -- Add multiple countries, making sure that you add
   --   + Countries from multiple continents
   --   + Multiple countries in at least one continent
   --   + Multiple countries with red in their flag
   --   + At least one country with no red in their flag
   -- If you do not add every country, you want the map
   -- to know which ones are added.
   -- NOTE: Don't worry if anything is actually correct!

   --  Print everything in the map
   Put_Line ("=== Entire Map ===");

   --  Print how many countries are on each continent
   Put_Line ("=== Countries per Continent ===");
   for Continent in Types.Continents_T'Range loop
      for Country in Types.Countries_T loop
         --  Increment a counter if this country is on this continent
         null;
      end loop;
      --  Print the continent name and how many countries
   end loop;

   --  Print how many countries have red on their flag
   Put_Line ("=== Flags with Red ===");
   for Country in Types.Countries_T loop
      --  Increment a counter if this country has red on its flag
      null;
   end loop;

end Main;
