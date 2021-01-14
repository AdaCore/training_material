.. code:: ada
    :class: ada-syntax-only

   package Aspect_Clauses is
      Eight_Bits : Integer range 0 .. 255 with
         Size => 8;
      Object : Integer with
         Atomic;
   end Aspect_Clauses;
