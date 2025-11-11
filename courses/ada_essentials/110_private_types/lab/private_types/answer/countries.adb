package body Countries is

   --|countries_1_begin
   function Find
     (Map : Map_T;
      Key : Key_T)
      return Integer is
   begin
      for I in 1 .. Map.Length loop
         if Map.Values (I).Country = Key and then Map.Values (I).Valid then
            return I;
         end if;
      end loop;
      return -1;
   end Find;

   procedure Add
     (Map       : in out Map_T;
      Country   :        Key_T;
      Continent :        Continent_T;
      Colors    :        Color_Set.Color_Set_T) is
      Index : constant Integer := Find (Map, Country);
   begin
      if Index not in Map.Values'Range then
         declare
            New_Item : constant Map_Component_T :=
              (Country   => Country,
               Valid     => True,
               Continent => Continent,
               Colors    => Colors);
         begin
            Map.Length              := Map.Length + 1;
            Map.Values (Map.Length) := New_Item;
         end;
      end if;
   end Add;

   function Exists
     (Map     : Map_T;
      Country : Key_T)
      return Boolean is (Find (Map, Country) in Map.Values'Range);
   --|countries_1_end

   --|countries_2_begin
   function Get
     (Map     : Map_T;
      Country : Key_T)
      return Map_Component_T is
      Index   : constant Integer := Find (Map, Country);
      Ret_Val : Map_Component_T;
   begin
      if Index in Map.Values'Range then
         Ret_Val := Map.Values (Index);
      end if;
      return Ret_Val;
   end Get;

   function Is_Valid (Component : Map_Component_T) return Boolean is
      (Component.Valid);
   function Colors (Component : Map_Component_T)
                    return Color_Set.Color_Set_T is
      (Component.Colors);
   function Continent (Component : Map_Component_T)
                       return Types.Continent_T is
      (Component.Continent);
   function Country (Component : Map_Component_T)
                     return Types.Country_T is
      (Component.Country);

   function Image (Item : Map_Component_T) return String is
     (Item.Country'Image & " => " & Color_Set.Image (Item.Colors));

   function Image (Map : Map_T) return String is
      Ret_Val : String (1 .. 1_000);
      Next    : Integer := Ret_Val'First;
   begin
      for I in 1 .. Map.Length loop
         declare
            Item : constant Map_Component_T := Map.Values (I);
            Str  : constant String          := Image (Item);
         begin
            Ret_Val (Next .. Next + Str'Length) := Image (Item) & ASCII.LF;
            Next                                := Next + Str'Length + 1;
         end;
      end loop;
      return Ret_Val (1 .. Next - 1);
   end Image;
   --|countries_2_end

end Countries;
