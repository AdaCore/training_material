==============
Introduction
==============

---------------
Strong Typing
---------------

* We know Ada supports strong typing

   .. code:: Ada

      type Small_Integer_T is range -1_000 .. 1_000;
      type Enumerated_T is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
      type Array_T is array (1 .. 3) of Boolean;

* But what if we need stronger enforcement?

   * Number must be even
   * Subset of non-consecutive enumerals
   * Array should always be sorted

* **Type Invariant**

   * Property of type that is always true on external reference
   * *Guarantee* to client, similar to subprogram postcondition

* **Subtype Predicate**

   * Add more complicated constraints to a type
   * Always enforced, just like other constraints

