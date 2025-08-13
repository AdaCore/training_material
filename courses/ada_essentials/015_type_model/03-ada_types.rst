===========
Ada Types
===========

--------------------
Ada "Named Typing"
--------------------

* **Name** differentiates types
* Structure does **not**
* Identical structures may **not** be interoperable

   .. code:: Ada

      type Yen is range 0 .. 100_000_000;
      type Ruble is range 0 .. 100_000_000;
      Mine : Yen;
      Yours : Ruble;
      ...
      Mine := Yours; -- not legal

.. note::

   Ada doesn't just *suggest* types - it enforces them like a
   hall monitor with a ruler

---------------------
Categories of Types
---------------------

.. image:: types_tree_complete.svg

---------------------------------
Understanding Types vs Subtypes
---------------------------------

* **Type** defines a distinct set of values and operations

* :dfn:`Subtype` restricts the range of values from a base type

   * Doesn't define a new type.

.. code:: Ada

   type Temperature is range -273 .. 5000;
   subtype Celsius is Temperature range -273 .. 100;

:color-white:`Blank space`

.. note::

   Subtypes are Ada's way of saying, "Yes, but not all the values, please."

