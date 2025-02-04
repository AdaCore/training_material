=================================
Standard Mathematical Functions
=================================

-----------------------------------
Ada.Numerics.Elementary_Functions
-----------------------------------

* Contains standard math and trigonometric functions

* All floating point parameters use standard floating point type :ada:`Float`

* Exceptions raised

  * When an invalid value is used (e.g. :ada:`Sqrt (-1)`), the exception :ada:`Argument_Error` is raised
  * When a value exceeds it's constraints, the normal :ada:`Constraint_Error` is raised

--------------------------------------------
Ada.Numerics.Elementary_Functions Contents
--------------------------------------------

* Standard math functions

  .. list-table::

    * - :ada:`"**"`

      - Allows exponents to be floating point

    * - :ada:`Sqrt`

      - Square root

    * - :ada:`Log`

      - Logarithm (one form for base *e* and one for user-specified base)

    * - :ada:`Exp`

      - *e* to the specified power

* Trigonometric functions

  .. list-table::

    * - :ada:`Arccos`

      - :ada:`Arcsin`

      - :ada:`Cos`

      - :ada:`Sin`

    * - :ada:`Arccosh`

      - :ada:`Arcsinh`

      - :ada:`Cosh`

      - :ada:`Sinh`

    * - :ada:`Arccot`

      - :ada:`Arctan`

      - :ada:`Cot`

      - :ada:`Tan`

    * - :ada:`Arccoth`

      - :ada:`Arctanh`

      - :ada:`Coth`

      - :ada:`Tanh`

-------------------------------------------
Ada.Numerics.Generic_Elementary_Functions
-------------------------------------------

* :ada:`Ada.Numerics.Generic_Elementary_Functions` is a generic package of mathematical functions

  * Can be instantiated for any floating point type

* :ada:`Ada.Numerics.Elementary_Functions` is actually just an instantiation of :ada:`Ada.Numerics.Generic_Elementary_Functions` with :ada:`Standard.Float`

