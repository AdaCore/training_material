*************
Basic Types
*************

..
    Coding language

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

..
    Math symbols

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

================
Introduction
================

----------------
Ada Type Model
----------------

* :dfn:`Static` Typing

   - Object type **cannot change**

* :dfn:`Strong` Typing

   - By **name**
   - **Compiler-enforced** operations and values
   - **Explicit** conversion for "related" types
   - **Unchecked** conversions possible

---------------
Strong Typing
---------------

* Definition of :dfn:`type`

   - Applicable **values**
   - Applicable :dfn:`primitive` **operations**

* Compiler-enforced

   - **Check** of values and operations
   - Easy for a computer
   - Developer can focus on **earlier** phase: requirement

-------------------------
Ada "Named Typing"
-------------------------

* **Name** differentiate types
* Structure does **not**
* Identical structures may **not** be interoperable

   .. code:: Ada

      type Yen is range 0 .. 100_000_000;
      type Ruble is range 0 .. 100_000_000;
      Mine : Yen;
      Yours : Ruble;
      ...
      Mine := Yours; -- not legal

---------------------
Categories of Types
---------------------

.. image:: types_tree.png

-----------
Attributes
-----------

* Functions *associated* with a type

   - May take input parameters

* Some are language-defined

    - *May* be implementation-defined
    - **Built-in**
    - Cannot be user-defined
    - Cannot be modified

* See RM K.2 *Language-Defined Attributes*
* Syntax

  .. code:: Ada

    Type_Name'Attribute_Name;
    Type_Name'Attribute_With_Param (Param);

* **'** often named *tick*

========================
Discrete Numeric Types
========================

-----------
Examples
-----------

.. include:: examples/030_basic_types/discrete_numeric_types.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/030_basic_types.html#discrete-numeric-types`

----------------------
Signed Integer Types
----------------------

* Range of signed **whole** numbers

   - Symmetric about zero (-0 = +0)

* Syntax

   .. code:: Ada

      type <identifier> is range  <lower> .. <upper>;

* Implicit numeric operators

   .. code:: Ada

      -- 12-bit device
      type Analog_Conversions is range 0 .. 4095;
      Count : Analog_Conversions;
      ...
      begin
         ...
         Count := Count + 1;
         ...
      end;

--------------------------------
Specifying Integer Type Bounds
--------------------------------

* Must be **static**

   - Compiler selects **base type**
   - Hardware-supported integer type
   - Compilation **error** if not possible

-----------------
Integer Overflows
-----------------

* Finite binary representation
* Common source of bugs

.. code:: Ada

   K : Short_Integer := Short_Integer'Last;
   ...
   K := K + 1;

    2#0111_1111_1111_1111#  = (2**16)-1

   +                    1

   =======================
    2#1000_0000_0000_0000#  = -32,768

----------------------------------
Range Attributes For All Scalars
----------------------------------

* :ada:`T'First`

  - First (**smallest**) value of type :ada:`T`

* :ada:`T'Last`

  - Last (**greatest**) value of type :ada:`T`

* :ada:`T'Range`

  - Shorthand for :ada:`T'First .. T'Last`

.. code:: Ada

   type Signed_T is range -99 .. 100;
   Smallest : Signed_T := Signed_T'First; -- -99
   Largest  : Signed_T := Signed_T'Last;  -- 100

-------------------------------------
Neighbor Attributes For All Scalars
-------------------------------------

* :ada:`T'Pred (Input)`

   - Predecessor of specified value
   - :ada:`Input` type must be :ada:`T`

* :ada:`T'Succ (Input)`

   - Successor of specified value
   - :ada:`Input` type must be :ada:`T`

.. code:: Ada

   type Signed_T is range -128 .. 127;
   type Unsigned_T is mod 256;
   Signed   : Signed_T := -1;
   Unsigned : Unsigned_T := 0;
   ...
   Signed := Signed_T'Succ(Signed); -- Signed = 0
   ...
   Unsigned := Unsigned_T'Pred(Unsigned); -- Signed = 255

====================
Enumeration Types
====================

-------------------
Enumeration Types
-------------------

* Enumeration of **logical** values

    - Integer value is an implementation detail

* Syntax

   .. code:: Ada

      type <identifier> is (<identifier-list>) ;

* Literals

   - Distinct, ordered
   - Can be in **multiple** enumerations

   .. code:: Ada

      type Colors is (Red, Orange, Yellow, Green, Blue, Violet);
      type Stop_Light is (Red, Yellow, Green);
      ...
      -- Red both a member of Colors and Stop_Light
      Shade : Colors := Red;
      Light : Stop_Light := Red;

-----------------------------
Enumeration Type Operations
-----------------------------

* Assignment, relationals
* **Not** numeric quantities

   - *Possible* with attributes
   - Not recommended

.. code:: Ada

   type Directions is (North, South, East, West);
   type Days is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
   Heading : Directions;
   Today, Tomorrow : Days;
   ...
   Today := Mon;
   Today := North; -- compile error
   Heading := South;
   Heading := East + 1; -- compile error
   if Today < Tomorrow then ...

-------------------------------
Language-Defined Type Boolean
-------------------------------

* Enumeration

   .. code:: Ada

      type Boolean is (False, True);

* Supports assignment, relational operators, attributes

   .. code:: Ada

      A : Boolean;
      Counter : Integer;
      ...
      A := (Counter = 22);

* Logical operators :ada:`and`, :ada:`or`, :ada:`xor`, :ada:`not`

   .. code:: Ada

      A := B or (not C); -- For A, B, C boolean

-----------------------------
Short-Circuit Control Forms
-----------------------------

* **Short-circuit** |rightarrow| **fixed** evaluation order
* Left-to-right
* Right only evaluated **if necessary**

   - :ada:`and then`: if left is :ada:`False`, skip right

     .. code:: Ada

        Divisor /= 0 and then K / Divisor = Max

   - :ada:`or else`: if left is :ada:`True`, skip right

     .. code:: Ada

        Divisor = 0 or else K / Divisor = Max

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
      + *Note* :ada:`Float'Rounding (0.5) = 1` and :ada:`Float'Rounding (-0.5) = -1`

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

=====
Lab
=====

.. include:: labs/030_basic_types.lab.rst

=========
Summary
=========

--------------------------------------
 Benefits of Strongly Typed Numerics
--------------------------------------

* **Prevent** subtle bugs
* Cannot mix :ada:`Apples` and :ada:`Oranges`
* Force to clarify **representation** needs

    - eg. constant with or with fractional part

   .. code:: Ada

      type Yen is range 0 .. 1_000_000;
      type Ruble is range 0 .. 1_000_000;
      Mine : Yen := 1;
      Yours : Ruble := 1;
      Mine := Yours; -- illegal

------------------------------------
User-Defined Numeric Type Benefits
------------------------------------

* Close to **requirements**

   - Types with **explicit** requirements (range, precision, etc.)
   - Best case: Incorrect state **not possible**

* Either implemented/respected or rejected

   - No run-time (bad) suprise

* **Portability** enhanced

   - Reduced hardware dependencies
