package Database is

   procedure Add (I_Value : Integer);
   procedure Add (F_Value : Float);
   procedure Add (C_Value : Character);

   procedure Remove (I_Value : Integer);
   procedure Remove (F_Value : Float);
   procedure Remove (C_Value : Character);

   procedure Print;

end Database;
