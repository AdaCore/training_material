===================
Tagged Derivation
===================

-----------------------------------
Difference with Simple Derivation
-----------------------------------

* Tagged derivation **can** change the structure of a type

    - Keywords :ada:`tagged record` and :ada:`with record`

      .. code:: Ada

         type Root_T is tagged record
            F1 : Integer;
         end record;

         type Child_T is new Root_T with record
            F2 : Integer;
         end record;

         Root_Object  : Root_T := (F1 => 101);
         Child_Object : Child_T := (F1 => 201, F2 => 202);

----------------
Type Extension
----------------

* A tagged derivation **has** to be a type extension

    - Use :ada:`with null record` if there are no additional components

   .. code:: Ada

      type Child_T is new Root_T with null record;
      type Child_T is new Root_T; -- illegal

* Conversion is only allowed from **child to parent**

  .. code:: Ada

         Root : Root_T;
         Child : Child_T;
         ...
         Root := Root_T (Child);
         Child := Child_T (Root); -- illegal

-----------------
Prefix Notation
-----------------

* Tagged types primitives can be called as usual
* The call can use prefixed notation

    - **If** the first argument is a controlling parameter
    - No need for :ada:`use` or :ada:`use type` for visibility

.. code:: Ada

         -- Prim1 visible even without *use Pkg*
         Object.Prim1;

         declare
            use Pkg;
         begin
            Prim1 (Object);
         end;

.. note::

  This is also called :dfn:`distinguished receiver` notation.
