-- Which piece(s) of code is a legal constructor for :ada:`T`?

procedure Main is
   --$ begin question
   type T is limited record
      I : Integer;
   end record;
   --$ end question

   --$ begin cut
   function F return T is
   begin
      return T (I => 0);
   end F;
   --$ end cut

   --$ begin cut
   function F return T is
      Val : Integer := 0;
   begin
      return (I => Val);
   end F;
   --$ end cut

   --$ begin cut
   function F return T is
      Ret : T := (I => 0);
   begin
      return Ret;
   end F;
   --$ end cut

   --$ begin cut
   function F return T is
   begin
      return (0);
   end F;
   --$ end cut

begin
   null;
end Main;
