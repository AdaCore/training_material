package Readability is

   type T_Mixed_Case is tagged null record;
   type Mixed_Case_Type is tagged null record;

   type A_Integer_Access is access Integer;
   type Integer_Access_Type is access Integer;

   type T_Tagged_Type is tagged null record;
   type A_Class_Access is access T_Tagged_Type'class;
   type Access_Class_Type is access T_Tagged_Type'class;

   Number : constant := 1_234;
   object : integer;

end Readability;
