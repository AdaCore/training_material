==================
Array Aggregates
==================

------------
Aggregates
------------

* Literals for composite types

   - Array types
   - Record types

* Two distinct forms

    - Positional
    - Named

* Syntax (simplified):

   .. code:: Ada

      component_expr ::=
        expression -- Defined value
        | <>       -- Default value

      array_aggregate ::= (
          {component_expr ,}                         -- Positional
        | {discrete_choice_list => component_expr,}) -- Named
        -- Default "others" indices
        [others => expression]

-----------------------------
Aggregate "Positional" Form
-----------------------------

* Specifies array component values explicitly
* Uses implicit ascending index values

.. code:: Ada

   type Days is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
   type Working is array (Days) of Boolean;
   Week : Working;
   ...
   -- Saturday and Sunday are False, everything else true
   Week := (True, True, True, True, True, False, False);

------------------------
Aggregate "Named" Form
------------------------

* Explicitly specifies both index and corresponding component values
* Allows any order to be specified
* Ranges and choice lists are allowed (like case choices)

.. code:: Ada

   type Days is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
   type Working is array (Days) of Boolean;
   Week : Working;
   ...
   Week := (Sat => False, Sun => False, Mon..Fri => True);
   Week := (Sat | Sun => False, Mon..Fri => True);

--------------------------------------
Combined Aggregate Forms Not Allowed
--------------------------------------

* Some cases lead to ambiguity, therefore never allowed for array types
* Are only allowed for record types (shown in subsequent section)

.. code:: Ada

   type Days is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
   type Working is array (Days) of Boolean;
   Week : Working;
   ...
   Week := (True, True, True, True, True, False, False);
   Week := (Sat => False, Sun => False, Mon..Fri => True);
   Week := (True, True, True, True, True,
            Sat => False, Sun => False); -- invalid
   Week := (Sat | Sun => False, Mon..Fri => True);

------------------------------------
Aggregates Are True Literal Values
------------------------------------

* Used any place a value of the type may be used

.. code:: Ada

   type Schedule is array (Mon .. Fri) of Float;
   Work : Schedule;
   Normal : constant Schedule := (8.0, 8.0, 8.0, 8.0, 8.0);
   ...
   Work := (8.5, 8.5, 8.5, 8.5, 6.0);
   ...
   if Work = Normal then
   ...
   if Work = (10.0, 10.0, 10.0, 10.0, 0.0) then -- 4-day week

-----------------------------
Aggregate Consistency Rules
-----------------------------

* Must always be complete

   - They are literals, after all
   - Each component must be given a value
   - But defaults are possible (more in a moment)

* Must provide only one value per index position

   - Duplicates are detected at compile-time

* Compiler rejects incomplete or inconsistent aggregates

   .. code:: Ada

      Week := (Sat => False,
               Sun => False,
               Mon .. Fri => True,
               Wed => False);

.. container:: speakernote

   Wednesday already covered in Monday .. Friday

-----------
 "Others"
-----------

* Indicates all components not yet assigned a value
* All remaining components get this single value
* Similar to case statement's :ada:`others`
* Can be used to apply defaults too

.. code:: Ada

   type Schedule is array (Days) of Float;
   Work : Schedule;
   Normal : constant Schedule := (8.0, 8.0, 8.0, 8.0, 8.0,
                                  others => 0.0);

-------------------
Nested Aggregates
-------------------

* For arrays of composite component types

.. code:: Ada

   type Col_T is array (1 .. 3) of Float;
   type Matrix_T is array (1 .. 3) of Col_T;
   Matrix : Matrix_T := (1 =>  (1.2, 1.3, 1.4),
                         2 =>  (2.5, 2.6, 2.7),
                         3 =>  (3.8, 3.9, 3.0));

----------------------------------
Defaults Within Array Aggregates
----------------------------------

* Specified via the ``box`` notation
* Value for component is thus taken as for stand-alone object declaration

   - So there may or may not be a defined default!

* Can only be used with "named association" form

   - But :ada:`others` counts as named form

* Syntax

   .. code:: Ada

      discrete_choice_list => <>

* Example

   .. code:: Ada

      type Int_Arr is array (1 .. N) of Integer;
      Primes : Int_Arr := (1 => 2, 2 .. N => <>);

..
  language_version 2005

------------------------------
Named Format Aggregate Rules
------------------------------

* Bounds cannot overlap

   - Index values must be specified once and only once

* All bounds must be static

   - Avoids run-time cost to verify coverage of all index values
   - Except for single choice format

.. code:: Ada

   type Float_Arr is array (Integer range <>) of Float;
   Ages : Float_Arr (1 .. 10) := (1 .. 3 => X, 4 .. 10 => Y);
   -- illegal: 3 and 4 appear twice
   Overlap : Float_Arr (1 .. 10) := (1 .. 4 => X, 3 .. 10 => Y);
   N, M, K, L : Integer;
   -- illegal: cannot determine if
   -- every index covered at compile time
   Not_Static : Float_Arr (1 .. 10) := (M .. N => X, K .. L => Y);
   -- This is legal
   Values : Float_Arr (1 .. N) := (1 .. N => X);

------
Quiz
------

.. code:: Ada

   type Array_T is array (1 .. 5) of Integer;
   X : Array_T;
   J : Integer := X'First;

Which statement is correct?

   A. ``X := (1, 2, 3, 4 => 4, 5 => 5);``
   B. :answermono:`X := (1..3 => 100, 4..5 => -100, others => -1);`
   C. ``X := (J => -1, J + 1..X'Last => 1);``
   D. ``X := (1..3 => 100, 3..5 => 200);``

.. container:: animate

   Explanations

   A. Cannot mix positional and named notation
   B. Correct - others not needed but is allowed
   C. Dynamic values must be the only choice. (This could be fixed by making :ada:`J` a constant.)
   D. Overlapping index values (3 appears more than once)

------------------------
Aggregates in Ada 2022
------------------------

.. admonition:: Language Variant

   Ada 2022

* Ada 2022 allows us to use square brackets **"[...]"** in defining aggregates

   .. code:: Ada

      type Array_T is array (positive range <>) of Integer;

   * So common aggregates can use either square brackets or parentheses

      .. code:: Ada

         Ada2012 : Array_T := (1, 2, 3);
         Ada2022 : Array_T := [1, 2, 3];

* But square brackets help in more problematic situations

   * Empty array

      .. code:: Ada

         Ada2012 : Array_T := (1..0 => 0);
         Illegal : Array_T := ();
         Ada2022 : Array_T := [];

   * Single component array

      .. code:: Ada

         Ada2012 : Array_T := (1 => 5);
         Illegal : Array_T := (5);
         Ada2022 : Array_T := [5];

--------------------------------
Iterated Component Association
--------------------------------

.. admonition:: Language Variant

   Ada 2022

* With Ada 2022, we can create aggregates with :dfn:`iterators`

   * Basically, an inline looping mechanism

* Index-based iterator

   .. code:: Ada

      type Array_T is array (positive range <>) of Integer;
      Object1 : Array_T(1..5) := (for J in 1 .. 5 => J * 2);
      Object2 : Array_T(1..5) := (for J in 2 .. 3 => J,
                                  5 => -1,
                                  others => 0);

   * :ada:`Object1` will get initialized to the squares of 1 to 5
   * :ada:`Object2` will give the equivalent of :ada:`(0, 2, 3, 0, -1)`

* Component-based iterator

   .. code:: Ada

      Object2 := [for Item of Object => Item * 2];

   * :ada:`Object2` will have each component doubled

-------------------------------
More Information on Iterators
-------------------------------

.. admonition:: Language Variant

   Ada 2022

* You can nest iterators for arrays of arrays

   .. code:: Ada

      type Col_T is array (1 .. 3) of Integer;
      type Matrix_T is array (1 .. 3) of Col_T;
      Matrix : Matrix_T :=
         [for J in 1 .. 3 =>
            [for K in 1 .. 3 => J * 10 + K]];

* You can even use multiple iterators for a single dimension array

   .. code:: Ada

      Ada2012 : Array_T(1..5) := 
         [for I in 1 .. 2 => -1,
          for J in 4 ..5 => 1,
          others => 0];

* Restrictions

   * You cannot mix index-based iterators and component-based iterators in the same aggregate

   * You still cannot have overlaps or missing values

------------------
Delta Aggregates
------------------

.. admonition:: Language Variant

   Ada 2022

.. code:: Ada

   type Coordinate_T is array (1 .. 3) of Float;
   Location : constant Coordinate_T := (1.0, 2.0, 3.0);

* Sometimes you want to copy an array with minor modifications

   * Prior to Ada 2022, it would require two steps

      .. code:: Ada

         declare
            New_Location : Coordinate_T := Location;
         begin
            New_Location(3) := 0.0;
            -- OR
            New_Location := (3 => 0.0, others => <>);
         end;

* Ada 2022 introduces a :dfn:`delta aggregate`

   * Aggregate indicates an object plus the values changed - the *delta*

      .. code:: Ada

         New_Location : Coordinate_T := [Location with delta 3 => 0.0];

* Notes

   * You can use square brackets or parentheses
   * Only allowed for single dimension arrays

*This works for records as well (see that chapter)*

