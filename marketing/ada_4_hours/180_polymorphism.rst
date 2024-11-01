**************
Polymorphism
**************

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
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==================
Classes of Types
==================

----------
Classes
----------

* In Ada, a Class denotes an inheritance subtree
* Class of `T` is the class of `T` and all its children
* Type :ada:`T'Class` can designate any object typed after type of class of `T`

   .. code:: Ada

      type Root is tagged null record;
      type Child1 is new Root with null record;
      type Child2 is new Root with null record;
      type Grand_Child1 is new Child1 with null record;
      -- Root'Class = {Root, Child1, Child2, Grand_Child1}
      -- Child1'Class = {Child1, Grand_Child1}
      -- Child2'Class = {Child2}
      -- Grand_Child1'Class = {Grand_Child1}

* Objects of type :ada:`T'Class` have at least the properties of T

   - Fields of `T`
   - Primitives of `T`

----------------
Abstract Types
----------------

* A tagged type can be declared :ada:`abstract`
* Then, :ada:`abstract tagged` types:

   - cannot be instantiated
   - can have abstract subprograms (with no implementation)
   - Non-abstract derivation of an abstract type must override and implement abstract subprograms

----------------------------
'Class and Prefix Notation
----------------------------

Prefix notation rules apply when the first parameter is of a class wide type

.. code:: Ada

   type Root is tagged null record;
   procedure P (V : Root'Class);
   type Child is new Root with null record;

   V1 : Root;
   V2 : Root'Class := Root'(others => <>);
   ...
   P (V1);
   P (V2);
   V1.P;
   V2.P;

===============================
Dispatching and Redispatching
===============================

---------------------------------
Calls on class-wide types (1/2)
---------------------------------

* Any subprogram expecting a T object can be called with a :ada:`T'Class` object

.. code:: Ada

   type Root is tagged null record;
   procedure P (V : Root);

   type Child is new Root with null record;
   procedure P (V : Child);

      V1 : Root'Class := [...]
      V2 : Child'Class := [...]
   begin
      P (V1);
      P (V2);

---------------------------------
Calls on class-wide types (2/2)
---------------------------------

* The *actual* type of the object is not known at compile time
* The *right* type will be selected at runtime

.. container:: columns

 .. container:: column

   *Ada*

      .. code:: Ada

         declare
           V1 : Root'Class :=
                Root'(others => <>);
           V2 : Root'Class :=
                Child'(others => <>);
         begin
           V1.P; -- calls P of Root
           V2.P; -- calls P of Child

 .. container:: column

   *C++*

      .. code:: C++

         Root * V1 = new Root ();
         Root * V2 = new Child ();
         V1->P ();
         V2->P ();
