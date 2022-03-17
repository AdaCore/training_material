package Object_Orientation is

   package Level1 is
      type T_Tagged_Type is tagged null record;
   end Level1;

   package Level2 is
      type T_Tagged_Type is new Level1.T_Tagged_Type with null record;
   end Level2;

   package Level3 is
      type T_Tagged_Type is new Level2.T_Tagged_Type with null record;
   end Level3;

   package Level4 is
      type T_Tagged_Type is new Level3.T_Tagged_Type with null record;
   end Level4;

   package Level5 is
      type T_Tagged_Type is new Level4.T_Tagged_Type with null record;
   end Level5;

   package Level6 is
      type T_Tagged_Type is new Level5.T_Tagged_Type with null record;
   end Level6;

   package Level7 is
      type T_Tagged_Type is new Level6.T_Tagged_Type with null record;
   end Level7;

end Object_Orientation;
