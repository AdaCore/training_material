package body Database is

   function Create (Number : Positive;
                    Symbol : Character)
                    return Database_T is
      Retval : constant Database_T :=
        (Number => Number,
         Symbol => Symbol);
   begin
      return Retval;
   end Create;

   function Image (Value : Database_T) return String is
   begin
      return Value.Symbol & Positive'Image (Value.Number);
   end Image;

   function "<" (Left, Right : Database_T) return Boolean is
   begin
      if Left.Symbol < Right.Symbol then
         return True;
      elsif Left.Symbol > Right.Symbol then
         return False;
      else
         return Left.Number < Right.Number;
      end if;
   end "<";

end Database;
