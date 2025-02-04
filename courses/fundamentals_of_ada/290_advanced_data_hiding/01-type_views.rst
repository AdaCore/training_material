============
Type Views
============

---------------------------------------
Capabilities / Constraints of a Type
---------------------------------------

* :dfn:`Constraints` in a type declaration

    - Reduce the set of operations available on a type
    - :ada:`limited`
    - Discriminants
    - :ada:`abstract`

* :dfn:`Capabilities` in a type declaration

    - Extends or modifies the set of operations available on a type
    - :ada:`tagged`
    - Tagged extensions

--------------------------------
Partial Vs Full View of a Type
--------------------------------

* If the partial view declares **capabilities**, the full view **must provide** them

   - Full view may provide supplementary capabilities undeclared in the partial view

* If the full has **constraints**, the partial view **must declare** them

   - Partial view may declare supplementary constraint that the full view doesn't have

.. code:: Ada

   package P is
      type T is limited private;
      -- Does not need to declare any capability
      -- Declares a constraint: limited
   private
      type T is tagged null record;
      -- Declares a capability: tagged
      -- Does not need to declare any constraint
   end P;

---------------
Discriminants
---------------

* Discriminants with no default must be declared both on the partial and full view

   .. code:: Ada

      package P is
         type T (V : Integer) is private;
      private
         type T (V : Integer) is null record;
      end P;

* Discriminants with default (in the full view) may be omitted by the partial view

   .. code:: Ada

      package P is
         type T1 (V : Integer := 0) is private;
         type T2 is private;
      private
         type T1 (V : Integer := 0) is null record;
         type T2 (V : Integer := 0) is null record;
      end P;

--------------------
Unknown Constraint
--------------------

* It is possible to establish that the type is unconstrained without any more information
* Definite and indefinite types can complete the private declaration

.. code:: Ada

   package P is
      type T1 (<>) is private;
      type T2 (<>) is private;
      type T3 (<>) is private;
   private
      type T1 (V : Integer) is null record;
      type T2 is array (Integer range <>) of Integer;
      type T3 is range 1 .. 10;
   end P;

---------
Limited
---------

* Limited property can apply only to the partial view
* If the full view is implicitly limited, the partial view has to be explicitly limited

.. code:: Ada

   package P is
      type T1 is limited private;
      type T2 is limited private;
      type T3 is limited private;
   private
      type T1 is limited null record;
      type T2 is record
         V : T1;
      end record;
      type T3 is range 1 .. 10;
   end P;

--------
Tagged
--------

* If the partial view is tagged, the full view has to be tagged
* The partial view can hide the fact that the type is tagged in the full view

   .. code:: Ada

      package P is
         type T1 is private;
         type T2 is tagged private;
         type T3 is tagged private;
      private
         type T1 is tagged null record;
         type T2 is tagged null record;
         type T3 is new T2 with null record;
      end P;

--------------------
Private Primitives
--------------------

* Primitives can be either public or private
* Privacy is **orthogonal** with type hierarchy

    - Derived types **may not** have access to private primitives
    - Child packages **can** access private part

        + and call the private primitive directly

* A primitive that has to be derived **must** be public

    - Abstract, constructor...

.. code:: Ada

    package P is
        type T is private;
        -- abstract must be public
        procedure Execute (Obj : T) is abstract;
        -- constructor must be public
        function Make return T;
    private
        procedure Internal_Reset (Obj : T); -- can be private
    end package P;

------------------
Tagged Extension
------------------

* The partial view may declare an extension
* The actual extension can be done on the same type, or on any of its children

.. code:: Ada

   package P is
      type Root        is tagged private;
      type Child       is new Root with private;
      type Grand_Child is new Root with private;
   private
      type Root        is tagged null record;
      type Child       is new Root with null record;
      type Grand_Child is new Child with null record;
   end P;

-----------------
Tagged Abstract
-----------------

* Partial view may be abstract even if Full view is not
* If Full view is abstract, private view has to be so

   .. code:: Ada

      package P is
         type T1 is abstract tagged private;
         type T2 is abstract tagged private;
      private
         type T1 is abstract tagged null record;
         type T2 is tagged null record;
      end P;

* Abstract primitives have to be public (otherwise, clients couldn't derive)

------------------
Protection Idiom
------------------

* It is possible to declare an object that can't be copied, and has to be initialized through a constructor function

   .. code:: Ada

      package P is
         type T (<>) is limited private;
         function F return T;
      private
         type T is null record;
      end P;

* Helps keeping track of the object usage

------
Quiz
------

.. include:: ../quiz/privacy_completion/quiz.rst

