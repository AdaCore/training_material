with Ada.Text_IO;
with Longest_Common_Prefix;

procedure Main
  with SPARK_Mode => On
is
begin
   Longest_Common_Prefix.A := (1, 2, 3, 4, 5, 1, 2, 3, 4, 5, others => 0);
   pragma Assert (Longest_Common_Prefix.A (1) = 1);
   pragma Assert (Longest_Common_Prefix.A (2) = 2);
   pragma Assert (Longest_Common_Prefix.A (1) /= Longest_Common_Prefix.A (2));
   pragma Assert (Longest_Common_Prefix.LCP (1, 2) = 0);
   -- pragma Assert (Longest_Common_Prefix.LCP (1001, 5) = 0);
   pragma Assert (Longest_Common_Prefix.LCP (10, 10) = 991);
   -- pragma Assert (Longest_Common_Prefix.LCP (10, 10) = 990);
   pragma Assert (Longest_Common_Prefix.LCP (5, 6) = 0);
   -- pragma Assert (Longest_Common_Prefix.LCP (5, 6) = 1);
   pragma Assert (Longest_Common_Prefix.LCP (4, 9) = 2);
   -- pragma Assert (Longest_Common_Prefix.LCP (4, 9) = 0);
   pragma Assert (Longest_Common_Prefix.LCP (9, 4) = 2);
end Main;
