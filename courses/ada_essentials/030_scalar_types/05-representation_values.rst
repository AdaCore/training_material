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

   * :ada:`'Enum_Rep` to get representation value

      :ada:`Charlie'Enum_Rep` |rightarrow| 63

   * :ada:`'Enum_Val` to convert integer to enumeral (if possible)

      :ada:`Enum_T'Enum_Val (15)` |rightarrow| :ada:`Baker`

      :ada:`Enum_T'Enum_Val (16)` |rightarrow| raise :ada:`Constraint_Error`

-----------------------------------------
Order Attributes for All Discrete Types
-----------------------------------------

* **All discrete** types, mostly useful for enumerated types
* :ada:`T'Pos (Input)`

   - "Logical position number" of :ada:`Input`

* :ada:`T'Val (Input)`

   - Converts "logical position number" to :ada:`T`

.. code:: Ada

   type Days is (Sun, Mon, Tue, Wed, Thu, Fri, Sat); -- 0 .. 6
   Today    : Days := Some_Value;
   Position : Integer;
   ...
   Position := Days'Pos (Today);
   ...
   Get (Position);
   Today := Days'Val (Position);

.. container:: speakernote

   Val/pos compared to value/image - same number of characters

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

