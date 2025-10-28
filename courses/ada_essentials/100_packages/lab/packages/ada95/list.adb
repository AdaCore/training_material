package body List is

   Global : array (1 .. 100) of Types.Record_T;
   Count  : Natural := 0;

   procedure Add
     (Left     : Types.Numeric_T;
      Operator : Character;
      Right    : Types.Numeric_T) is
   begin
      Count          := Count + 1;
      Global (Count) :=
        (Left     => Left,
         Right    => Right,
         Operator => Operator);
   end Add;

   function Length return Natural is
   begin
      return Count;
   end Length;

   function Element
     (Index : Integer)
      return Types.Record_T is
   begin
      return Global (Index);
   end Element;

end List;
