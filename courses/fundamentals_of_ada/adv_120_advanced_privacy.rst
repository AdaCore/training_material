******************
Advanced Privacy
******************

..
    Coding language

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

..
    Math symbols

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

============
Type Views
============

---------------------------------------
Capabilities / Constraints Of A Type
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
Partial Vs Full View Of A Type
--------------------------------

* If the partial view declares **capabilities**, the full view **must provide** them

   - Full view may provide supplementary capabilities undeclared in the partial view

* If the full has **constraints**, the partial view **must declare** them

   - Partial view may declare supplementary constraint that the full view doesn't have

.. include:: examples/adv_120_advanced_privacy/capabilities_and_constraints.adb
    :code: Ada

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
* Constrained and unconstrained types can complete the private declaration

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
        procedure Execute (Obj : T) is abstract; -- abstract must be public
        function Make return T; -- constructor must be public
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

.. include:: quiz/privacy_completion/quiz.rst

==================
Incomplete Types
==================

------------------
Incomplete Types
------------------

* An :dfn:`incomplete type` is a premature view on a type

   - Does specify the type name
   - Can specify the type discriminants
   - Can specify if the type is tagged

* It can be used in contexts where minimum representation information is required

   - In declaration of access types
   - In subprograms specifications (only if the body has full visibility on the representation)
   - As formal parameter of generics accepting an incomplete type

-------------------------------------
How To Get An Incomplete Type View?
-------------------------------------

* From an explicit declaration

   .. code:: Ada

      type T;
      type T_Access is access all T;
      type T is record
         V : T_Access;
      end record;

* From a :ada:`limited with` (see section on packages)
* From an incomplete generic formal parameter (see section on generics)

   .. code:: Ada

      generic
         type T;
         procedure Proc (V:T);
      package P is
         ...
      end P;

--------------------------------------
Type Completion Deferred To The Body
--------------------------------------

* In the private part of a package, it is possible to defer the completion of an incomplete type to the body
* This allows to completely hide the implementation of a type

.. code:: Ada

   package P is
      ...
   private
      type T;
      procedure P (V : T);
      X : access T;
   end P;
   package body P is
      type T is record
         A, B : Integer;
      end record;
      ...
   end P;

------
Quiz
------

.. include:: quiz/incomplete_type/quiz.rst

------
Quiz
------

.. include:: quiz/private_incomplete/quiz.rst

=======================
Private Library Units
=======================

-------------------------
Child Units And Privacy
-------------------------

* Normally, a child public part cannot view a parent private part

  .. container:: columns

    .. container:: column

    .. container:: column

      .. code:: Ada

        package Root is
        private
           type T is range 1..10;
        end Root;

    .. container:: column

      .. code:: Ada

        package Root.Child is
          X1 : T; -- illegal
        private
          X2 : T;
        end Root.Child;

* :dfn:`Private child` can view the private part

    + Used for "implementation details"

---------------------------
Importing a Private Child
---------------------------

* A :ada:`private package` can view its **parent** :ada:`private` part
* View on a private package is restricted

   + To the *Private descendents of their parent*
   + Visible from parent's :ada:`body`
   + Visible from public sibling's :ada:`private` section, and :ada:`body`
   + Visible from private siblings (public, :ada:`private`, :ada:`body`)

  .. container:: columns

    .. container:: column

      .. code:: Ada

        package Root is
        private
          type T is range 1..10;
        end Root;

    .. container:: column

      .. code:: Ada

        private package Root.Child is
          X1 : T;
        private
          X2 : T;
        end Root.Child;

        with Root.Child; -- illegal
        procedure Main is
        begin
           Root.Child.X1 := 10; -- illegal
        end Main;

----------------------------------
Private Children And :ada:`with`
----------------------------------

.. code:: Ada

   private package Root.Child1 is
      type T is range 1 .. 10;
   end Root.Child1;

* Public package cannot :ada:`with` a private package

   .. code:: Ada

      with Root.Child1; -- illegal
      package Root.Child2 is
         X1 : Root.Child1.T; -- illegal
      private
         X2 : Root.Child1.T; -- illegal
      end Root.Child2;

* Child packages can :ada:`with` a sibling private package

    + From their body only

   .. code:: Ada

      with Root.Child1;
      private package Root.Child2 is
         X1 : Root.Child1.T;
      private
         X2 : Root.Child1.T;
      end Root.Child2;


---------------------
:ada:`private with`
---------------------

* The parent and its children can :ada:`private with` a private package

    + From anywhere
    + View given **stays** :ada:`private`

   .. code:: Ada

      private with Root.Child1;
      package Root.Child2 is
         X1 : Root.Child1.T; -- illegal
      private
         X2 : Root.Child1.T;
      end Root.Child2;

* Clients of :ada:`Root.Child2` don't have any visibility on :ada:`Root.Child1`

------------------------------------------------------------
Children "Inherit" From Private Properties Of Parent
------------------------------------------------------------

* Private property always refers to the direct parent
* Public children of private packages stay private to the outside world
* Private children of private packages restrain even more the accessibility

.. code:: Ada

   package Root is
   end Root;

   private package Root.Child is
     --  with allowed on Root body
     --  with allowed on Root children
     --  with forbidden outside of Root
   end Root.Child;

   package Root.Child.Grand1 is
     --  with allowed on Root body
     --  with allowed on Root children
     --  with forbidden outside of Root
   end Root.Child.Grand1;

   private package Root.Child.Grand2 is
     --  with allowed on Root.Child body
     --  with allowed on Root.Child children
     --  with forbidden outside of Root.Child
     --  with forbidden on Root
     --  with forbidden on Root children
   end Root.Child1.Grand2;

========
Lab
========

.. include:: labs/adv_120_advanced_privacy.lab.rst

=========
Summary
=========

---------
Summary
---------

* Ada has many mechanisms for data hiding / control
* Start by fully understanding supplier / client relationship
* Need to balance simplicity of interfaces with complexity of structure

   - Small number of relationship per package with many packages
   - Fewer packages with more relationships in each package
   - No set standard

      * Varies from project to project
      * Can even vary within a code base

