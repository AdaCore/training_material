==============
Types in Ada
==============

---------------------------
Ada's Strong Typing Model
---------------------------

* Ada is *strongly* and *statically* typed

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

----------------------
Type System Spectrum
----------------------

.. list-table::
   :header-rows: 1

   * - Language
     - Static Typing
     - Strong Typing
     - Implicit Conversion

   * - Ada
     - :math:`\textcolor{green!65!black}{\checkmark}`
     - :math:`\textcolor{green!65!black}{\checkmark}` (very)
     - :color-red:`X`

   * - C/C++
     - :math:`\textcolor{green!65!black}{\checkmark}`
     - :color-red:`X`
     - :math:`\textcolor{green!65!black}{\checkmark}`

   * - Python
     - :color-red:`X`
     - :math:`\textcolor{green!65!black}{\checkmark}`
     - :math:`\textcolor{green!65!black}{\checkmark}`

   * - Rust
     - :math:`\textcolor{green!65!black}{\checkmark}`
     - :math:`\textcolor{green!65!black}{\checkmark}`
     - :color-red:`X`

   * - Java
     - :math:`\textcolor{green!65!black}{\checkmark}`
     - :math:`\textcolor{green!65!black}{\checkmark}` (mostly)
     - :color-red:`X`

   * - JavaScript
     - :color-red:`X`
     - :color-red:`X`
     - :math:`\textcolor{green!65!black}{\checkmark}`

---------------------------
Type Model Run-Time Costs
---------------------------

* Checks at compilation **and** run-time

* Good code requires ranges to be verified

   - By user writing the checks **OR**
   - By compiler inserting them

      - Sometimes compiler can even flag failures

.. container:: latex_environment small

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
   affected by how the checks are inserted

----------------------------
The Type Model Saves Money
----------------------------

* Shifts fixes and costs to **early phases**

* Cost of an error *during a flight*?

.. image:: relative_cost_to_fix_bugs.svg
