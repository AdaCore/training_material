-- Which declaration(s) is(are) legal?
procedure Main is
   --$ begin question
   generic
      type T is tagged;
      type T2;
   procedure G_P;

   type Tag is tagged null record;
   type Arr is array (Positive range <>) of Tag;
   --$ end question

   procedure G_P is
   begin
      null;
   end G_P;

   --$ line cut
   procedure P is new G_P (Tag, Arr);
   --$ line cut
   procedure P is new G_P (Arr, Tag);
   --$ line cut
   procedure P is new G_P (Tag, Tag);
   --$ line cut
   procedure P is new G_P (Arr, Arr);

begin
   null;
end Main;
