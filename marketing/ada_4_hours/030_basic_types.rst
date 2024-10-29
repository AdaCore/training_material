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

-------------------------
Ada "Named Typing"
-------------------------

* **Name** differentiate types
* Structure does **not**
* Identical structures may **not** be interoperable

   .. code:: Ada

      type Yen is range 0 .. 100_000_000;
      type Kilometers is range 0 .. 100_000_000;
      Money : Yen;
      Distance : Kilometers;
      ...
      Money := Distance; -- not legal

-----------
Attributes
-----------

* Functions *associated* with a type

   - May take input parameters

* Some are language-defined

    - *May* be implementation-defined
    - **Built-in**
    - Cannot be user-defined
    - Some can be overridden

* Examples

  .. code:: Ada

    Typemark'Size
    Integer'Max (A, B);

========================
Discrete Numeric Types
========================

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

============
Real Types
============

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
