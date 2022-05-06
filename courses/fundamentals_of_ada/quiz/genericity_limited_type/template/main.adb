-- Which declaration(s) is(are) legal?

procedure Main is
   --$ begin question
   generic
      type L is limited private;
      type P is private;
   procedure G_P;
   
   type Lim is limited null record;
   type Int is new Integer;

   type Rec is record
      L : Lim;
      I : Int;
   end record;
   --$ end question

   procedure G_P is
   begin
      null;
   end G_P;

   --$ line cut
   procedure P is new G_P (Lim, Int);
   --$ line cut
   procedure P is new G_P (Int, Rec);
   --$ line cut
   procedure P is new G_P (Rec, Rec);
   --$ line cut
   procedure P is new G_P (Int, Int);

begin
   null;
end Main;
