with GNAT.Random_Numbers;

package body Random_Numbers is

   Gen : GNAT.Random_Numbers.Generator;

   function Random (From, To : Integer) return Integer is
      Candidate : Integer;
   begin
      loop
         Candidate := GNAT.Random_Numbers.Random (Gen);
         exit when Candidate in From .. To;
      end loop;
      return Candidate;
   end Random;

begin
   GNAT.Random_Numbers.Reset (Gen);
end Random_Numbers;
