package Database is
   type Database_T is private;

   function Create (Number : Positive;
                    Symbol : Character)
                    return Database_T;

   function Image (Value : Database_T) return String;

   function "<" (Left, Right : Database_T) return Boolean;

private
   type Database_T is record
      Number : Positive;
      Symbol : Character;
   end record;
end Database;
