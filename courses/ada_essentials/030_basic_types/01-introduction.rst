================
Introduction
================

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
