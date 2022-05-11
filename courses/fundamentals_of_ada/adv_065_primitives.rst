*********************
Advanced Primitives
*********************

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
Primitives
============

-------------
Inheritance
-------------

--------------
Freeze Point
--------------

* Ada doesn't explicitly identify the end of members declaration
* This end is the implicit **freeze point** occurring whenever:

   - A **variable** of the type is **declared**
   - The type is **derived**
   - The **end of the scope** is reached

* Subprograms past this point are not primitive

.. code:: Ada

   type Root is Integer;
   procedure Prim (V : Root);
   type Child is new Root; -- freeze root
   procedure Prim2 (V : Root); -- Not a primitive

   V : Child; --  freeze child
   procedure Prim3 (V : Child); -- Not a primitive

----------------------
What is type freeze?
----------------------

* :command:`-gnatDG`

=================
Type Derivation
=================

------------------------------
Primitive of Multiple Types
------------------------------

* A subprogram can be a primitive of several types

      .. code:: Ada

         package P is
            type T1 is range 1 .. 10;
            type T2 is (A, B, C);

            procedure Proc (V1 : T1; V2 : T2);
            function "+" (V1 : T1; V2 : T2) return T1;
         end P;

-------------------------------
Implicit Primitive Operations
-------------------------------

* Type declaration implicitly creates primitives

    - Numerical and logical operations
    - Code can overload or remove them

   .. code:: Ada

      package P is
         type T1 is range 1 .. 10;
         -- implicit
         -- function "+" (Left, Right : T1) return T1;
      end P;
      ...
      procedure Main is
         V1, V2 : T1;
      begin
         V1 := V1 + V2;
      end Main;

---------------------------
Recap. on type derivation
---------------------------

* For all types

    + Freeze point rules don't change
    + Primitives are inherited by child types
    + Conversion from child to parent possible
    + Pre-defined set of primitives

        - ``"+", "-" ...`` for numeric types
        - Comparison operators
        - Equality except if :ada:`limited`

* Derived type that are **not** :ada:`tagged`

    + Are **not** OOP
    + Can remove a primitive
    + Can declare a primitive of multiple types
    + Can be converted from parent to child

        - Their representation does not change
        - Could raise :ada:`Constraint_Error` (:ada:`range`...)

------
Quiz
------

.. include:: quiz/operators_override_simple/quiz.rst

------
Quiz
------

.. include:: quiz/derivation_op_override/quiz.rst

====================
Tagged Inheritance
====================

---------------------------------
Liskov's Substitution Principle
---------------------------------

* :dfn:`LSP` is an object-oriented rule

    + Not imposed

        - But fits nicely with Ada's OOP design

    + Avoids numerous issues
    + Can be verified by tools eg. :toolname:`CodePeer`

* *Objects of a parent type shall be replaceable by objects of its child types*

    + Cannot be applied to simple derivation (eg. restricting range)
    + Tagged record derivation implies **extending** not modifying the behaviour

        - Easier said than done
        - *Is a mute cat still a cat if it can't meow?*

-------------
Dispatching
-------------

* Primitives dispatch, but not only them

.. include:: examples/adv_065_primitives/dispatching.1.rst

* :ada:`Prim` is a primitive
* :ada:`Not_Prim` is **not** a primitive

    + Won't be inherited
    + But dispatches dynamically!

.. include:: examples/adv_065_primitives/dispatching.2.rst

------------------------------
Tagged Primitive Declaration
------------------------------

* :ada:`tagged` types primitives **must** be declared in a :ada:`package` specification
* Not a :ada:`declare` block or the declarative part of a subprogram

.. include:: examples/adv_065_primitives/subprogram_primitive_declaration.rst

-----------------------------
Primitive of Multiple Types
-----------------------------

* For a primitive of a :ada:`tagged record Tag_T`

    - :ada:`Tag_T` is called the :dfn:`controlling parameter`
    - All controlling parameters **must** be of the same type

* Warning: A non-tagged type is never controlling

    - Can have primitive of multiple :ada:`type`
    - **Cannot** have primitive of multiple :ada:`tagged record`

.. code:: Ada

    type Root1 is tagged null record;
    type Root2 is tagged null record;

    procedure P1_Correct (V1 : Root1; V2 : Root1);
    procedure P2_Incorrect (V1 : Root1; V2 : Root2); -- FAIL

------------------------------
Recap. on tagged inheritance
------------------------------

* :ada:`tagged` types are Ada's OOP
* They can

    + Be converted from a parent: :ada:`Child_Type (Parent)`

* They **cannot**

    + Remove a primitive
    + Have a primitive with multiple controlling types
    + Be converted to a parent: :ada:`Parent_Type (Child)`

        - Because their representation may change

------
Quiz
------

.. include:: quiz/multiple_primitive/quiz.rst

------
Quiz
------

.. include:: quiz/primitives_and_classwide/quiz.rst
