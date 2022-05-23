package body Examples is

   procedure Test_Statement
     (A, B, C :     Integer;
      Z       : out Integer) is
      Local : Integer := A;
   begin
      Local := Local * 10;
      Local := Local + B * 100;
      if C > 0
      then
         Local := Local * 2;
         Z     := Local + C * 1_000;
      end if;
   end Test_Statement;

   procedure Test_Decision
     (A, B, C :     Integer;
      Z       : out Integer) is
      Check : constant Boolean := A > 0 and then (B**2 > 0 or else C**2 > 0);
   begin
      if Check
      then
         Z := A + B + C;
      else
         Z := A * B * C;
      end if;
   end Test_Decision;

   procedure Test_Mcdc
     (A, B, C :     Integer;
      Z       : out Integer) is
   begin
      if A > 0 and then B > 0
      then
         Z := A * B;
      end if;
      if A > 0 or else B > 0 or else C > 0
      then
         Z := Z + A + B + C;
      end if;
   end Test_Mcdc;

end Examples;
