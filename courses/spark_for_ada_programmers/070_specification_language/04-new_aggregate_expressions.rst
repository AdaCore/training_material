===========================
New Aggregate Expressions
===========================

------------------
Delta Aggregates
------------------

.. admonition:: Language Variant

   Ada 2022

* Express the value of a **modified** composite object (record or array)

  .. code:: Ada

     (Rec with delta Comp1 => Val1, Comp2 => Val2)
     (Arr with delta 1 => True, 42 => False)

* Typically used to relate input and output **values** of parameters

  - Combines delta aggregate with use of attribute :ada:`'Old`

  .. code:: Ada

     procedure P (Rec : in out T)
       with Post => Rec = (Rec'Old with delta Comp1 => Val1,
                                              Comp2 => Val2);

* With array object:

  - Avoids the introduction of **explicit** quantifiers
  - Can have **overlapping** and **dynamic** choices (values or ranges)

-----------------------------
Extension of Delta Aggregates
-----------------------------

.. admonition:: Language Variant

   GNAT Extension

* GNAT extension allowed using either one of

  - switch :command:`-gnatX0`
  - pragma :ada:`Extensions_Allowed (All)`
    
* Choice can be a subcomponent of the record or array

  .. code:: Ada

     (Rec with delta Comp1.Sub1 => Val1,
                     Comp2.Sub2.Subsub => Val2)
     (Arr with delta (1).Sub => True,
                     (42).Subarr(4) => False)

---------------------------------
Iterated Component Associations
---------------------------------

.. admonition:: Language Variant

   Ada 2022

* Express the **value** of an array aggregate depending on index
* Example: the *identity* function

  .. code:: Ada

     (for J in T'Range => J)

* This is a :dfn:`component association`

  - Can be used in **any** aggregate
  - Can be mixed with regular component associations :ada:`Idx => Val`

----------------------
Container Aggregates
----------------------

.. admonition:: Language Variant

   Ada 2022

* Available for all functional and formal containers

* Vectors, lists and sets use the positional syntax:

  .. code:: Ada

     V : Vector := [1, 2, 3];
     L : List := [1, 2, 3];
     S : Set := [1, 2, 3];

* Maps use the named syntax:

  .. code:: Ada

     M : Map := [1 => 8, 4 => 3, 42 => 127];

* General mechanism using the :ada:`Container_Aggregates` annotation

  - Three predefined patterns :ada:`Predefined_Sequences`,
    :ada:`Predefined_Sets` and :ada:`Predefined_Maps` require specific API
    (used for functional containers)

  - :ada:`From_Model` only requires ``Model`` function returning the above
    (used for formal containers)

  - Consistency checked by :toolname:`GNATprove`

