with GNAT.Random_Numbers;
package body Odometer is

   Generator : Gnat.Random_Numbers.Generator;

   function Read return Base_Types.Meters_T is
      Distance : constant Float :=
        Gnat.Random_Numbers.Random (Generator) * 10.0;
   begin
      return Base_Types.Meters_T (Distance);
   end Read;

begin

   Gnat.Random_Numbers.Reset (Generator);

end Odometer;
