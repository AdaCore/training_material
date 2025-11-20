with Ada.Text_IO;
with Database;
with GNAT.Random_Numbers;
with Table;

procedure Main is

   Generator : GNAT.Random_Numbers.Generator;

   function Dice_Roll is new
     GNAT.Random_Numbers.Random_Discrete
       (Database.Dice_T,
        Database.Dice_T'First);
   function Attribute return Database.Attribute_T
   is ((Dice_Roll (Generator), Dice_Roll (Generator), Dice_Roll (Generator)));

begin
   GNAT.Random_Numbers.Reset (Generator);
   Table.Initialize;
   for Outer in 1 .. 10 loop
      for Inner in 1 .. 100 loop
         Database.Increment (Attribute);
      end loop;
      Ada.Text_IO.New_Line;
      Database.Print ("Table");
      Table.Print ("Order");
   end loop;
end Main;
