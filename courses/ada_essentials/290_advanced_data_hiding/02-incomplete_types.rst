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
How to Get an Incomplete Type View?
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
         with procedure Proc (V : T);
      package P is
         ...
      end P;

--------------------------------------
Type Completion Deferred to the Body
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

.. include:: ../quiz/incomplete_type/quiz.rst

------
Quiz
------

.. include:: ../quiz/private_incomplete/quiz.rst

