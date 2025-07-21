.. code:: ada

   package Stack_Pkg is
      procedure Push (Item : in Integer) with
            Pre  => not Full,
            Post => not Empty and then Top = Item;
      procedure Pop (Item : out Integer) with
            Pre  => not Empty,
            Post => not Full and Item = Top'Old;
      function Pop return Integer with
            Pre  => not Empty,
            Post => not Full and Pop'Result = Top'Old;
      function Top return Integer with
            Pre => not Empty;
      function Empty return Boolean;
      function Full return Boolean;
   end Stack_Pkg;
