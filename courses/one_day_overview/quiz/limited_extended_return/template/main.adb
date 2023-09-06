-- Which declaration(s) of ``F`` is(are) valid?
procedure Main is
   --$ begin question
   type T is limited record
      I : Integer;
   end record;

   function F return T is
   begin
      -- F body...
   --$ end question

      --$ begin cut
      return Return : T := (I => 1);
      -- Using :ada:`return` reserved keyword
      --$ end cut

      --$ begin cut
      return Result : T;
      -- OK, default value
      --$ end cut

      --$ begin cut
      return Value := (others => 1);
      -- Extended return must specify type
      --$ end cut

      --$ begin cut
      return R : T do
         R.I := 1;
      end return;
      -- OK
      --$ end cut

   --$ begin question
   end F;

   O : T := F;
   --$ end question
begin
   null;
end Main;
