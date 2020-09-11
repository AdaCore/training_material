with Longest_Common_Prefix;

procedure Main
  with SPARK_Mode => On
is
begin
   Longest_Common_Prefix.A := (1, 2, 3, 4, 5, 1, 2, 3, 4, 5, others => 0);
end Main;
