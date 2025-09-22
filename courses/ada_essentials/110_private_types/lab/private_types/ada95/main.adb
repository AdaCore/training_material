with Ada.Text_IO; use Ada.Text_IO;
with Color_Set;
with Countries;
with Types;       use Types;
procedure Main is
   Map : Countries.Map_T;
   One : Countries.Map_Component_T;
begin

   Countries.Add
     (Map       => Map,
      Country   => Types.United_States,
      Continent => Types.North_America,
      Colors    => Color_Set.Create
          (Colors =>
             (Red,
              White,
              Blue)));

   Countries.Add
     (Map       => Map,
      Country   => Types.Finland,
      Continent => Types.Europe,
      Colors    => Color_Set.Create
          (Colors =>
             (Blue,
              White)));

   Countries.Add
     (Map       => Map,
      Country   => Types.New_Zealand,
      Continent => Types.Australia,
      Colors    => Color_Set.Create
          ((Red,
            White,
            Blue)));

   Put_Line ("=== Entire Map ===");
   Put_Line (Countries.Image (Map));

   New_Line;
   Put_Line ("=== Countries per Continent ===");
   declare
      Count : Natural;
   begin
      for Continent in Types.Continents_T'Range loop
         Count := 0;
         for Country in Types.Countries_T loop
            One := Countries.Get (Map, Country);
            if Countries.Is_Valid (One)
              and then Countries.Continent (One) = Continent
            then
               Count := Count + 1;
            end if;
         end loop;
         Put_Line
           (Types.Continents_T'Image (Continent) & " => " &
            Natural'Image (Count));
      end loop;
   end;

   New_Line;
   Put_Line ("=== Flags with Red ===");
   declare
      Red : Natural := 0;
   begin
      for Country in Types.Countries_T loop
         One := Countries.Get (Map, Country);
         if Countries.Is_Valid (One)
           and then Color_Set.Contains (Countries.Colors (One), Types.Red)
         then
            Red := Red + 1;
         end if;
      end loop;
      Put_Line ("Flags with red => " & Natural'Image (Red));
   end;

end Main;
