=======================
Representation Values
=======================

-----------------------------------
Enumeration Representation Values
-----------------------------------

* Numeric **representation** of enumerals

  - Position, unless redefined
  - Redefinition syntax

    .. code:: Ada

        type Enum_T is (Able, Baker, Charlie, David);
        for Enum_T use
          (Able => 3, Baker => 15, Charlie => 63, David => 255);

  - Enumerals are ordered **logically** (not by value)

* Prior to Ada 2022

  - Only way to get value is through :ada:`Unchecked_Conversion`

    .. code:: Ada

         function Value is new Ada.Unchecked_Conversion
            (Enum_T, Integer_8);
         I : Integer_8;

    .. code:: Ada

         begin
            I := Value (Charlie);

* New attributes in Ada 2022 

  * :ada:`'Enum_Rep`

    * Representation value for enumeral
    * :ada:`Charlie'Enum_Rep` |rightarrow| **63**

  * :ada:`'Enum_Val`

    * Convert integer to enumeral (if possible)
    * :ada:`Enum_T'Enum_Val (15)` |rightarrow| :ada:`Baker`
    * :ada:`Enum_T'Enum_Val (16)` |rightarrow| raise :ada:`Constraint_Error`

-------------------------------------
Order Attributes for Discrete Types
-------------------------------------

.. code:: Ada

   type Numbers_T is (One, Three, Five, Four, Two);

* **All** discrete types, mostly useful for enumerated types

* :ada:`T'Pos (Input)`

   - Logical position number of :ada:`Input`
   - :ada:`Numbers_T'Pos(Two)` |rightarrow| **4**

* :ada:`T'Val (Input)`

   - Converts logical position number to :ada:`T`
   - :ada:`Numbers_T'Pos(3)` |rightarrow| :ada:`Four`

------
Quiz
------

.. code:: Ada

    type Direction_T is (Left, Top, Right, Bottom);
    Dir : Direction_T := Left;

Which of the following proposition(s) are true?

A. ``Direction_T'Value (Dir) = 1``
B. :answermono:`Direction_T'Pos (Dir) = 0`
C. ``Direction_T'Image (Direction_T'Pos (Dir)) = Left``
D. ``Direction_T'Val (Direction_T'Pos (Dir) - 1) = Bottom``

.. container:: animate

   Explanations

   A. :ada:`'Value` converts an image to a string
   B. :ada:`'Pos` returns position in enumeration starting at 0
   C. :ada:`'Image` returns a string
   D. Argument of :ada:`'Val` must be in range - here it's -1

