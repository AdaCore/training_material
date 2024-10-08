============
Real Types
============

------------
Real Types
------------

* Approximations to **continuous** values

  - 1.0, 1.1, 1.11, 1.111 ... 2.0, ...
  - Finite hardware |rightarrow| approximations

* Floating-point

  - **Variable** exponent
  - **Large** range
  - Constant **relative** precision

* Fixed-point

  - **Constant** exponent
  - **Limited** range
  - Constant **absolute** precision
  - Subdivided into Binary and Decimal

* Class focuses on floating-point

------------------------------------------
Real Type (Floating and Fixed) Literals
------------------------------------------

* **Must** contain a fractional part
* No silent promotion

.. code:: Ada

   type Phase is digits 8; -- floating-point
   OK : Phase := 0.0;
   Bad : Phase := 0 ; -- compile error

--------------------------------
Declaring Floating Point Types
--------------------------------

* Syntax

    .. code:: Ada

       type <identifier> is
           digits <expression> [range constraint];

  - *digits* |rightarrow| **minimum** number of significant digits
  - **Decimal** digits, not bits

* Compiler choses representation

  - From **available** floating point types
  - May be **more** accurate, but not less
  - If none available |rightarrow| declaration is **rejected**

* :ada:`System.Max_Digits` - constant specifying maximum digits of precision available for runtime

  .. code:: Ada

    type Very_Precise_T is digits System.Max_Digits;

  *Need to do* :ada:`with System;` *to get visibility*

---------------------------------
Predefined Floating Point Types
---------------------------------

* Type :ada:`Float` ``>= 6`` digits
* Additional implementation-defined types

   - :ada:`Long_Float` ``>= 11`` digits

* General-purpose
* Best to **avoid** predefined types

   - Loss of **portability**
   - Easy to avoid

-------------------------------
Floating Point Type Operators
-------------------------------

* By increasing precedence

   :relational operator: :ada:`= | /= | < | >= | > | >=`
   :binary adding operator: :ada:`+ | -`
   :unary adding operator: :ada:`+ | -`
   :multiplying operator: :ada:`* | /`
   :highest precedence operator: :ada:`** | abs`

* *Note* on floating-point exponentiation ``**``

   - Power must be :ada:`Integer`

      + Not possible to ask for root
      + :ada:`X**0.5` |rightarrow| :ada:`sqrt (x)`

--------------------------------
Floating Point Type Attributes
--------------------------------

* *Core* attributes

   .. code:: Ada

      type My_Float is digits N;  -- N static

   - :ada:`My_Float'Digits`

      + Number of digits **requested** (N)

   - :ada:`My_Float'Base'Digits`

      + Number of **actual** digits

   - :ada:`My_Float'Rounding (X)`

      + Integral value nearest to :ada:`X`
      + *Note:* :ada:`Float'Rounding (0.5) = 1` and :ada:`Float'Rounding (-0.5) = -1`

* Model-oriented attributes

   - Advanced machine representation of the floating-point type
   - Mantissa, strict mode

---------------------------
Numeric Types Conversion
---------------------------

* Ada's integer and real are :dfn:`numeric`

    - Holding a numeric value

* Special rule: can always convert between numeric types

    - Explicitly
    - :ada:`Float` |rightarrow| :ada:`Integer` causes **rounding**

.. code:: Ada

   declare
      N : Integer := 0;
      F : Float := 1.5;
   begin
      N := Integer (F); -- N = 2
      F := Float (N); -- F = 2.0

------
Quiz
------

What is the output of this code?

.. code:: Ada

   declare
      F : Float := 7.6;
      I : Integer := 10;
   begin
      F := Float (Integer (F) / I);
      Put_Line (Float'Image (F));
   end;

A. 7.6
B. Compile Error
C. 8.0
D. :answer:`0.0`

.. container:: animate

   Explanations

   A. Result of :ada:`F := F / Float (I);`
   B. Result of :ada:`F := F / I;`
   C. Result of :ada:`F := Float (Integer (F)) / Float (I);`
   D. Integer value of :ada:`F` is 8. Integer result of dividing that by 10 is 0. Converting to float still gives us 0

