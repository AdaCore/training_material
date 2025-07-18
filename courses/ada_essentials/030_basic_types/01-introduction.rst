================
Introduction
================

---------------
Strong Typing
---------------

* Definition of :dfn:`type`

   - Applicable **values**
   - Applicable :dfn:`primitive` **operations**

* Compiler-enforced

   - **Check** of values and operations
   - Easy for a computer

.. tip:: Developer can focus on **earlier** phase: requirement

----------------------------------------
Strongly-Typed Vs Weakly-Typed Languages
----------------------------------------

* Weakly-typed:

    - Conversions are **unchecked**
    - Type errors are easy

.. code:: C++

   typedef enum {north, south, east, west} direction;
   typedef enum {sun, mon, tue, wed, thu, fri, sat} days;
   direction heading = north;

   heading = 1 + 3 * south/sun;// what?

* Strongly-typed:

    - Conversions are **checked**
    - Type errors are hard

.. code:: Ada

   type Directions is (North, South, East, West);
   type Days is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
   Heading : Directions := North;
   ...
   Heading := 1 + 3 * South/Sun; --  Compile Error

----------------------
A Little Terminology
----------------------

* **Declaration** creates a **type identifier**

   .. code:: Ada

      type <identifier> is <type definition>;

* **Type-definition** defines its structure

   - Characteristics, and operations
   - Base "class" of the type

   .. code:: Ada

      type Type_1 is digits 12; -- floating-point
      type Type_2 is range -200 .. 200; -- signed integer
      type Type_3 is mod 256; -- unsigned integer

* :dfn:`Representation` is the memory-layout of an **object** of the type

---------------------------
Abstract Data Types (ADT)
---------------------------

* **Variables** of the **type** encapsulate the **state**
* Classic definition of an ADT

   - Set of **values**
   - Set of **operations**
   - **Hidden** compile-time **representation**

* Compiler-enforced

   - Check of values and operation
   - Easy for a computer
   - Developer can focus on **earlier** phase: requirements

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

--------------
Scalar Types
--------------

* Indivisible: No :dfn:`components` (also known as *fields* or *elements*)
* **Relational** operators defined (``<``,  ``=``, ...)

    - **Ordered**

* Have common **attributes**
* **Discrete** Types

  - Integer
  - Enumeration

* **Real** Types

  - Floating-point
  - Fixed-point

----------------
Discrete Types
----------------

* **Individual** ("discrete") values

   - 1, 2, 3, 4 ...
   - Red, Yellow, Green

* Integer types

   - Signed integer types
   - Modular integer types

      * Unsigned
      * **Wrap-around** semantics
      * Bitwise operations

* Enumeration types

   - Ordered list of **logical** values

-----------
Attributes
-----------

* Properties of entities that can be queried like a function

   - May take input parameters

* Defined by the language and/or compiler

    - Language-defined attributes found in RM K.2
    - *May* be implementation-defined

       * GNAT-defined attributes found in GNAT Reference Manual

    - Cannot be user-defined

* Attribute behavior is generally pre-defined

  - :ada:`Type_T'Digits` gives number of digits used in :ada:`Type_T` definition

* Some attributes can be modified by coding behavior

  - :ada:`Typemark'Size` gives the size of :ada:`Typemark`

    - Determined by compiler **OR** by using a representation clause

  - :ada:`Object'Image` gives a string representation of :ada:`Object`

    - Default behavior which can be replaced by aspect :ada:`Put_Image`

* Examples

  .. code:: Ada

    J := Object'Size;
    K := Array_Object'First(2);

---------------------------
Type Model Run-Time Costs
---------------------------

* Checks at compilation **and** run-time
* **Same performance** for identical programs

   - Run-time type checks can be disabled

.. note:: Compile-time check is *free*

.. container:: columns

 .. container:: column

   **C**

   .. code:: C++

      int X;
      int Y; // range 1 .. 10
      ...
      if (X > 0 && X < 11)
        Y = X;
      else
        // signal a failure

 .. container:: column

   **Ada**

   .. code:: Ada

      X : Integer;
      Y, Z : Integer range 1 .. 10;
      ...
      Y := X;
      Z := Y; -- no check required

--------------------------
The Type Model Saves Money
--------------------------

* Shifts fixes and costs to **early phases**

* Cost of an error *during a flight*?

.. image:: relative_cost_to_fix_bugs.jpg
   :height: 50%
