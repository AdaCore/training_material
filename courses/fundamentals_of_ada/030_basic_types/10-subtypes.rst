==========
Subtypes
==========

----------
Subtype
----------

* May **constrain** an existing type
* Still the **same** type
* Syntax

   .. code:: Ada

      subtype Defining_Identifier is Type_Name [constraints];

   - :ada:`Type_Name` is an existing :ada:`type` or :ada:`subtype`

.. note:: If no constraint |rightarrow| type alias

-----------------
Subtype Example
-----------------

* Enumeration type with :ada:`range` constraint

   .. code:: Ada

      type Days is (Sun, Mon, Tues, Wed, Thurs, Fri, Sat);
      subtype Weekdays is Days range Mon .. Fri;
      Workday : Weekdays; -- type Days limited to Mon .. Fri

* Equivalent to **anonymous** subtype

   .. code:: Ada

      Same_As_Workday : Days range Mon .. Fri;

----------------------
Kinds of Constraints
----------------------

* Range constraints on scalar types

   .. code:: Ada

      subtype Positive is Integer range 1 .. Integer'Last;
      subtype Natural is Integer range 0 .. Integer'Last;
      subtype Weekdays is Days range Mon .. Fri;
      subtype Symmetric_Distribution is
          Float range -1.0 .. +1.0;

* Other kinds, discussed later
* Constraints apply only to values
* Representation and set of operations are **kept**

---------------------------
Subtype Constraint Checks
---------------------------

* Constraints are checked

   - At initial value assignment
   - At assignment
   - At subprogram call
   - Upon return from subprograms

* Invalid constraints

   - Will cause :ada:`Constraint_Error` to be raised
   - May be detected at compile time

      + If values are **static**
      + Initial value |rightarrow| error
      + ... else |rightarrow| warning

.. code:: Ada

   Max : Integer range 1 .. 100 := 0; -- compile error
   ...
   Max := 0; -- run-time error

--------------------------------------------
Performance Impact of Constraints Checking
--------------------------------------------

* Constraint checks have run-time performance impact
* The following code

   .. code:: Ada

      procedure Demo is
        K : Integer := F;
        P : Integer range 0 .. 100;
      begin
        P := K;

* Generates assignment checks similar to

   .. code:: Ada

      if K < 0 or K > 100 then
        raise Constraint_Error;
      else
        P := K;
      end if;

* These checks can be disabled with :command:`-gnatp`

------------------------------------
Optimizations of Constraint Checks
------------------------------------

* Checks happen only if necessary
* Compiler assumes variables to be **initialized**
* So this code generates **no check**

   .. code:: Ada

      procedure Demo is
        P, K : Integer range 0 .. 100;
      begin
        P := K;
        --  But K is not initialized!

---------------------------
Range Constraint Examples
---------------------------

.. code:: Ada

   subtype Proper_Subset is Positive range 1 .. 10;
   subtype Same_Constraints is Positive
       range 1 .. Integer'Last;
   subtype Letter is Character range 'A' .. 'z';
   subtype Upper_Case is Letter range 'A' .. 'Z';
   subtype Lower_Case is Letter range 'a' .. 'z';
   subtype Null_Range is Integer
       range 1 .. 0;  -- silly when hard-coded...
   -- evaluated when subtype defined, not when object declared
   subtype Dynamic is Integer range Lower .. Upper;

------
Quiz
------

.. code:: Ada

   type Days_Of_Week_T is (Sat, Sun, Mon, Tue, Wed, Thu, Fri);
   subtype Weekdays_T is Days_Of_Week_T range Mon .. Fri;

Which subtype definition is valid?

   A. ``subtype A is Weekdays_T range Weekdays_T'Pred (Weekdays_T'First) .. Weekdays_T'Last;``
   B. ``subtype B is range Sat .. Mon;``
   C. :answermono:`subtype C is Integer;`
   D. ``subtype D is digits 6;``

.. container:: animate

   Explanations

   A. This generates a run-time error because the first enumeral specified is not in the range of :ada:`Weekdays_T`
   B. Compile error - no type specified
   C. Correct - standalone subtype
   D. :ada:`Digits 6` is used for a type definition, not a subtype

