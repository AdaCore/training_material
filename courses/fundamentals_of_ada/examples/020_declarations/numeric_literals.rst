.. code:: ada
    :class: ada-run

   package Numeric_Literals is
   
      Simple_Integer  : constant := 3;
      Decimal_Number  : constant := 0.25;
      Using_Separator : constant := 1_000_000.0;
      Octal           : constant := 8#33#;
      Hexadecimal     : constant := 16#AAAA#;
   
   end Numeric_Literals;
