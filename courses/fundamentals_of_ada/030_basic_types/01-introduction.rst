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

----------------------
A Little Terminology
----------------------

* **Declaration** creates a **type name**

   .. code:: Ada

      type <name> is <type definition>;

* **Type-definition** defines its structure

   - Characteristics, and operations
   - Base "class" of the type

   .. code:: Ada

      type Type_1 is digits 12; -- floating-point
      type Type_2 is range -200 .. 200; -- signed integer
      type Type_3 is mod 256; -- unsigned integer

* :dfn:`Representation` is the memory-layout of an **object** of the type

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

* Indivisible: No components
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

