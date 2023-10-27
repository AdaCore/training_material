.. code:: ada

   package body Stack_Pkg is
      Values  : array (1 .. 100) of Integer;
      Current : Natural := 0;
      -- Preconditions prevent Push/Pop failure
      procedure Push (Item : in Integer) is
      begin
         Current          := Current + 1;
         Values (Current) := Item;
      end Push;
      procedure Pop (Item : out Integer) is
      begin
         Item    := Values (Current);
         Current := Current - 1;
      end Pop;
      function Pop return Integer is
         Item : constant Integer := Values (Current);
      begin
         Current := Current - 1;
         return Item;
      end Pop;
      function Top return Integer is
        (Values (Current));
      function Empty return Boolean is
        (Current not in Values'Range);
      function Full return Boolean is
        (Current >= Values'Length);
   end Stack_Pkg;
