==============
Types in Ada
==============

---------------------------
Ada's Strong Typing Model
---------------------------

* Ada is **strongly** and **statically** typed

   * Types are checked at compile-time, not run-time

* Every object has a specific type

   * Explicit conversions of similar types are allowed

* Type safety is a core design goal

   * Prevents accidental operations between incompatible types

------------------------------------------
Strongly-Typed Vs Weakly-Typed Languages
------------------------------------------

* Weakly-typed

    - Conversions are **unchecked**
    - Type errors are easy

.. code:: C++

   typedef enum {north, south, east, west} direction;
   typedef enum {sun, mon, tue, wed, thu, fri, sat} days;
   direction heading = north;

   heading = 1 + 3 * south/sun;  // what?

* Strongly-typed

    - Conversions are **checked**
    - Type errors are hard

.. code:: Ada

   type Directions is (North, South, East, West);
   type Days is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
   Heading : Directions := North;
   ...
   Heading := 1 + 3 * South/Sun; --  Compile Error
