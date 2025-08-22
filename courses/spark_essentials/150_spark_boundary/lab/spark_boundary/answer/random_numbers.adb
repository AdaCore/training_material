with GNAT.Random_Numbers;

package body Random_Numbers
  with SPARK_Mode => Off
is

   Gen : GNAT.Random_Numbers.Generator;

   procedure Random (From, To : Integer; Result : out Integer) is
      Candidate : Integer;
   begin
      loop
         Candidate := GNAT.Random_Numbers.Random (Gen);
         exit when Candidate in From .. To;
      end loop;
      Result := Candidate;
   end Random;

begin
   GNAT.Random_Numbers.Reset (Gen);
end Random_Numbers;
