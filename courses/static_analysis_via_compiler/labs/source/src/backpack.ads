package Backpack is

   Useless   : constant := 0;
   Mandatory : constant := 5;
   type Utility is range Useless .. Mandatory;

   Boring    : constant := 0;
   So_Cooool : constant := 5;
   type Fun is range Boring .. So_Cooool;

   subtype Weight is Natural range 0 .. 10_000;  --  in grams
   Scoliosis_Limit : constant := 6_000;

   type Item is record
      U : Utility;
      F : Fun;
      W : Weight;
   end record;

   --  content of the backpack
   type Content is array (Positive range <>) of Item;

   type Belonging is record
      Something : Item;
      Available : Boolean;
   end record;

   type Extended is range 0 .. 100;
   subtype Index is Extended range 1 .. 100;

   --  set of stuff to choose from to populate the backpack
   type Belongings is array (Index) of Belonging;

   type Strategy is
     (Teacher,      --  maximize utility
      Mom_And_Pop,  --  maximize utility and minimize weight
      Kiddo);       --  maximize fun and avoid punishment

   --  Choose items from Stuff to populate Choice according to Strat
   procedure Back_To_School
     (Strat  :        Strategy;
      Stuff  : in out Belongings;
      Choice : in out Content;
      Num    :    out Natural);

end Backpack;
