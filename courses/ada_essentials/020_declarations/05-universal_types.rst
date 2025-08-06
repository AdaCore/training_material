=================
Universal Types
=================

-----------------
Universal Types
-----------------

* Implicitly defined
* Entire *classes* of numeric types

   - :ada:`universal_integer`
   - :ada:`universal_real`
   - :ada:`universal_fixed` (not seen here)

* Match any integer / real type respectively

   - **Implicit** conversion, as needed

  .. code:: Ada

     X : Integer64 := 2;
     Y : Integer8 := 2;
     F : Float := 2.0;
     D : Long_Float := 2.0;

----------------------------------------
Numeric Literals Are Universally Typed
----------------------------------------

* No need to type them

   - e.g :code:`0UL` as in C

* Compiler handles typing

.. note:: No bugs with precision

.. code:: Ada

   X : Unsigned_Long := 0;
   Y : Unsigned_Short := 0;

----------------------------------------
Literals Must Match "Class" of Context
----------------------------------------

* `universal_integer` literals |rightarrow| **Integer**
* `universal_real` literals |rightarrow| **fixed** or **floating** point
* Legal

  .. code:: Ada

     X : Integer := 2;
     Y : Float := 2.0;

* Not legal

  .. code:: Ada

     X : Integer := 2.0;
     Y : Float := 2;

