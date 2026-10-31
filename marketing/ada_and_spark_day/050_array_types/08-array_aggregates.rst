==================
Array Aggregates
==================

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

----------
"Others"
----------

* Indicates all components not yet assigned a value
* All remaining components get this single value
* Similar to case statement's :ada:`others`
* Can be used to apply defaults too

.. code:: Ada

   type Schedule is array (Days) of Float;
   Work : Schedule;
   Normal : constant Schedule := (8.0, 8.0, 8.0, 8.0, 8.0,
                                  others => 0.0);
------------------------
Aggregates in Ada 2022
------------------------

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

------------------
Delta Aggregates
------------------

.. code:: Ada

   type Coordinate_T is array (1 .. 3) of Float;
   Location : constant Coordinate_T := (1.0, 2.0, 3.0);

* Sometimes an array needs to be copied with only minor modifications

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

   * Can use square brackets or parentheses
   * Only allowed for single dimension arrays
