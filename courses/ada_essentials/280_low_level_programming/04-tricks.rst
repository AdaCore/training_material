========
Tricks
========

--------------------
Package Interfaces
--------------------

* Package :ada:`Interfaces` provide Integer and unsigned types for many sizes

   - :ada:`Integer_8`, :ada:`Integer_16`, :ada:`Integer_32`, :ada:`Integer_64`
   - :ada:`Unsigned_8`, :ada:`Unsigned_16`, :ada:`Unsigned_32`, :ada:`Unsigned_64`

* With shift/rotation functions for unsigned types

------------------------------
Fat/Thin Pointers for Arrays
------------------------------

* Unconstrained array access is a fat pointer

   .. code:: Ada

      type String_Acc is access String;
      Msg : String_Acc;
      -- array bounds stored outside array pointer

* Use a size representation clause for a thin pointer

   .. code:: Ada

      type String_Acc is access String;
      for String_Acc'Size use 32;
      -- array bounds stored as part of array pointer

-------------
Flat Arrays
-------------

* A constrained array access is a thin pointer

   - No need to store bounds

   .. code:: Ada

      type Line_Acc is access String (1 .. 80);

* You can use big flat array to index memory

   - See :ada:`GNAT.Table`
   - Not portable

   .. code:: Ada

      type Char_array is array (natural) of Character;
      type C_String_Acc is access Char_Array;

