**************
Access Types
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

==========================
Access Types
==========================

---------------------
Access Types Design
---------------------

* Memory-addressed objects are called :dfn:`access types`

* C++

  .. code:: C++

     int *P_C =
           malloc (sizeof (int));
     int *P_CPP = new int;
     int *G_C = &Some_Int;

* Ada

  .. code:: Ada

     type Integer_General_Access
       is access all Integer;
     G : aliased Integer;
     G_A : Integer_General_Access := G'access;

-------------
Null Values
-------------

* A pointer that does not point to any actual data has a :ada:`null` value
* Access types have default value of :ada:`null`
* :ada:`null` can be used in assignments and comparisons

.. code:: Ada

   declare
      type Acc is access all Integer;
      V : Acc;
   begin
      if V = null then
         --  will go here
      end if
      V := new Integer'(0);
      V := null; -- semantically correct, but memory leak

----------------------------
Dereferencing Access Types
----------------------------

* :ada:`.all` does the access dereference

   - Lets you access the object pointed to by the pointer

* :ada:`.all` is optional for

   - Access on a component of an array
   - Access on a component of a record

.. code:: Ada

   type Record_T is record
     F1, F2 : Integer;
   end record;
   type Integer_Access_T is access Integer;
   type String_Access_T is access all String;
   type Record_Access_T is access all Record_T;

   Integer_Access : Integer_Access_T := new Integer;
   String_Access  : String_Access_T  := new String'("abc");
   Record_Access  : Record_Access_T  := new R;

   Integer_Access.all := 123;
   String_Access(1)   := "-";
   Record_Access.F1   := 456;
   Record_Access.all  := (7, 8);

==========================
General Access Types
==========================

----------------------
General Access Types
----------------------

* Can point to any pool (including stack)

   .. code:: Ada

      type T is [...]
      type T_Access is access all T;
      V : T_Access := new T;

* Still distinct type
* Conversions are possible

   .. code:: Ada

      type T_Access_2 is access all T;
      V2 : T_Access_2 := T_Access_2 (V); -- legal

-------------
Allocations
-------------

* Objects are created with the :ada:`new` reserved word
* The created object must be constrained

   - The constraint is given during the allocation

      .. code:: Ada

         V : String_Access := new String (1 .. 10);

* The object can be created by copying an existing object - using a qualifier

   .. code:: Ada

      V : String_Access := new String'("This is a String");

---------------
Deallocations
---------------

* Deallocations are unsafe

   - Multiple deallocations problems
   - Memory corruptions
   - Access to deallocated objects

* As soon as you use them, you lose the safety of your access
* But sometimes, you have to do what you have to do ...

   - There's no simple way of doing it
   - Ada provides `Ada.Unchecked_Deallocation`
   - Has to be instantiated (it's a generic)
   - Must work on an object, reset to :ada:`null` afterwards

-----------------------
Referencing The Stack
-----------------------

* By default, stack-allocated objects cannot be referenced - and can even be optimized into a register by the compiler
* :ada:`aliased` declares an object to be referenceable through an access value

   .. code:: Ada

      V : aliased Integer;

* :ada:`'Access` attribute gives a reference to the object

   .. code:: Ada

      A : Int_Access := V'Access;

   - :ada:`'Unchecked_Access` does it **without checks**
