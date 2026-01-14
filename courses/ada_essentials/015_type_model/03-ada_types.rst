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

   In Ada, types are like airport security: if your bag doesn't match
   the rules, you're not getting through

---------------------
Categories of Types
---------------------

.. image:: types_tree_complete.svg

---------------------------------
Understanding Types vs Subtypes
---------------------------------

* **Type** defines a distinct set of values and operations

* :dfn:`Subtype` (usually) restricts the range of values from a base type

   * Doesn't define a new type

     .. code:: Ada

        type Temperature is range -273 .. 5000;
        subtype Celsius is Temperature range -273 .. 100;

* Subtype that does **not** add a restriction is generally refered to as an *alias*

  .. code:: Ada

      subtype Water_Temperature is Temperature;

.. raw:: latex

  \vspace{5mm}

.. note::

   Subtypes are Ada's way of saying, "Yes, but not all the values, please."

