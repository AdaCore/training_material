-- Which of the following piece(s) of code is(are) legal?

procedure Main is
   --$ begin question
   generic
      type T is (<>);
      G_A : in out T;
   procedure G_P;

   type I is new Integer;
   type E is (OK, NOK);
   type F is new Float;
   X : I;
   Y : E;
   Z : F;
   --$ end question

   procedure G_P is
   begin
      null;
   end G_P;

   --$ line cut
   procedure P is new G_P (I, X);
   --$ line cut
   procedure P is new G_P (E, Y);
   --$ line cut
   procedure P is new G_P (I, E'Pos (Y));
   --$ line cut
   procedure P is new G_P (F, Z);

begin
   null;
end Main;
