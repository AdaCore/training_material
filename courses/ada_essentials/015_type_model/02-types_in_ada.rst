==============
Types in Ada
==============

---------------------------
Ada's Strong Typing Model
---------------------------

* Ada is *strongly* and *statically* typed

   * Types are checked at compile-time, not run-time

* Every object has a specific type

   * Implicit conversions are rare

* Type safety is a core design goal

   * Prevents accidental operations between incompatible types

------------------------------------------
Strongly-Typed Vs Weakly-Typed Languages
------------------------------------------

* Weakly-typed:

    - Conversions are **unchecked**
    - Type errors are easy

.. code:: C++

   typedef enum {north, south, east, west} direction;
   typedef enum {sun, mon, tue, wed, thu, fri, sat} days;
   direction heading = north;

   heading = 1 + 3 * south/sun;  // what?

* Strongly-typed:

    - Conversions are **checked**
    - Type errors are hard

.. code:: Ada

   type Directions is (North, South, East, West);
   type Days is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
   Heading : Directions := North;
   ...
   Heading := 1 + 3 * South/Sun; --  Compile Error

---------------------------
Type Model Run-Time Costs
---------------------------

* Checks at compilation **and** run-time

* Good code requires ranges to be verified

   - By user writing the checks **OR**
   - By compiler inserting them

      - Sometimes compiler can even flag failures

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

.. note::

   Checks need to be made, so performance shouldn't be
   affected by how the checks are inserted.

----------------------------
The Type Model Saves Money
----------------------------

* Shifts fixes and costs to **early phases**

* Cost of an error *during a flight*?

.. image:: relative_cost_to_fix_bugs.jpg
   :height: 50%

