=======================
Representation Values
=======================

-----------------------------------
Enumeration Representation Values
-----------------------------------

.. admonition:: Language Variant

   Ada 2022

* Numeric **representation** of enumerals

    - Position, unless redefined
    - Redefinition syntax

      .. code:: Ada

         type Enum_T is (Able, Baker, Charlie, Dog, Easy, Fox);
         for Enum_T use (1, 2, 4, 8, Easy => 16, Fox => 32);

* Prior to Ada 2022

   - Enumerals are ordered **logically** (not by value)
   - Only way to get value is through :ada:`Unchecked_Conversion`

* New attributes in Ada 2022 

   * :ada:`'Enum_Rep` to get representation value

      :ada:`Charlie'Enum_Rep` |rightarrow| 4

   * :ada:`'Enum_Val` to convert integer to enumeral (if possible)

      :ada:`Enum_T'Enum_Val (8)` |rightarrow| :ada:`Dog`

      :ada:`Enum_T'Enum_Val (9)` |rightarrow| raise :ada:`Constraint_Error`

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

    type T is (Left, Top, Right, Bottom);
    V : T := Left;

Which of the following proposition(s) are true?

A. ``T'Value (V) = 1``
B. :answermono:`T'Pos (V) = 0`
C. ``T'Image (T'Pos (V)) = Left``
D. ``T'Val (T'Pos (V) - 1) = Bottom``

