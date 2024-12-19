============
Primitives
============

--------------------
Primitive Operations
--------------------

* Primitive Operations are those subprograms associated with a type

.. code:: Ada

   type Integer_T is range -(2**63) .. 2**63-1 with Size => 64; 
   procedure Increment_With_Truncation (Val : in out Integer_T);
   procedure Increment_With_Rounding (Val : in out Integer_T);

* Most types have some primitive operations defined by the language

   * e.g. equality operators for most types, numeric operators for integers and floats

* A primitive operation on the parent can receive an object of a child type with no conversion

   .. code:: Ada

      declare
         N_Obj : Natural_T := 1234;
      begin
         Increment_With_Truncation (N_Obj);
      end;

---------------------------------------
General Rule for Defining a Primitive
---------------------------------------

* Primitives are subprograms
* Subprogram :ada:`S` is a primitive of type :ada:`T` if and only if:

   - :ada:`S` is declared in the scope of :ada:`T`
   - :ada:`S` uses type :ada:`T`

        + As a parameter
        + As its return type (for a :ada:`function`)

   - :ada:`S` is above :dfn:`freeze-point` (see next section)

* Standard practice

    - Primitives should be declared **right after** the type itself
    - In a scope, declare at most a **single** type with primitives

      .. code:: Ada

         package P is
            type T is range 1 .. 10;
            procedure P1 (V : T);
            procedure P2 (V1 : Integer; V2 : T);
            function F return T;
         end P;

------------------------------
Primitive of Multiple Types
------------------------------

A subprogram can be a primitive of several types

   .. code:: Ada

      package P is
         type Distance_T is range 0 .. 9999;
         type Percentage_T is digits 2 range 0.0 .. 1.0;
         type Units_T is (Meters, Feet, Furlongs);

         procedure Convert (Value  : in out Distance_T; 
                            Source :        Units_T;
                            Result :        Units_T;
         procedure Shrink (Value   : in out Distance_T;
                           Percent :        Percentage_T);

      end P;

* :ada:`Convert` and :ada:`Shrink` are primitives for :ada:`Distance_T`
* :ada:`Convert` is also a primitive of :ada:`Units_T`
* :ada:`Shrink` is also a primitive of :ada:`Percentage_T`

----------------------------------
Creating Primitives for Children
----------------------------------

* Just because we can inherit a primitive from out parent doesn't mean we want to

* We can create a new primitive (with the same name as the parent) for the child

   * Very similar to overloaded subprograms
   * But added benefit of visibility to grandchildren

* We can also remove a primitive (see next slide)

.. code:: Ada

   type Integer_T is range -(2**63) .. 2**63-1; 
   procedure Increment_With_Truncation (Val : in out Integer_T);
   procedure Increment_With_Rounding (Val : in out Integer_T);
   
   type Child_T is new Integer_T range -1000 .. 1000;
   procedure Increment_With_Truncation (Val : in out Child_T);
   
   type Grandchild_T is new Child_T range -100 .. 100;
   procedure Increment_With_Rounding (Val : in out Grandchild_T);

------------------------
Overriding Indications
------------------------

* **Optional** indications
* Checked by compiler

   .. container:: latex_environment footnotesize

      .. code:: Ada

         type Child_T is new Integer_T range -1000 .. 1000;
         procedure Increment_With_Truncation
            (Val : in out Child_T);
         procedure Just_For_Child
            (Val : in out Child_T);

* **Replacing** a primitive: :ada:`overriding` indication

   .. container:: latex_environment footnotesize

      .. code:: Ada

         overriding procedure Increment_With_Truncation
            (Val : in out Child_T);

* **Adding** a primitive: :ada:`not overriding` indication

   .. container:: latex_environment footnotesize

      .. code:: Ada

         not overriding procedure Just_For_Child
            (Val : in out Child_T);

* **Removing** a primitive: :ada:`overriding` as :ada:`abstract`

   .. container:: latex_environment footnotesize

      .. code:: Ada

         overriding procedure Just_For_Child
            (Val : in out Grandchild_T) is abstract;

* Using :ada:`overriding` or :ada:`not overriding` incorrectly will generate a compile error

..
  language_version 2005

------
Quiz
------

.. include:: ../quiz/operators_override_simple/quiz.rst
