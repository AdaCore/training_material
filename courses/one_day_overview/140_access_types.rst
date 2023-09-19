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

.. container:: columns

 .. container:: column

  * Ada

  .. code:: Ada

     type Integer_General_Access
       is access all Integer;
     G : aliased Integer;
     G_A : Integer_General_Access := G'access;

 .. container:: column

  * C++

  .. code:: C++

     int * P_C = malloc (sizeof (int));
     int * P_CPP = new int;
     int * G_C = &Some_Int;
.

----------------------
Declaration Location
----------------------

* Can be at library level

   .. code:: Ada

      package P is
        type String_Access is access String;
      end P;

* Can be nested in a procedure

   .. code:: Ada

      package body P is
         procedure Proc is
            type String_Access is access String;
         begin
            ...
         end Proc;
      end P;

* Nesting adds non-trivial issues

   - Creates a nested pool with a nested accessibility
   - Don't do that unless you know what you are doing!

-------------
Null Values
-------------

* A pointer that does not point to any actual data has a :ada:`null` value
* Without an initialization, a pointer is :ada:`null` by default
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

------------------------
Dereferencing Pointers
------------------------

* :ada:`.all` does the access dereference

   - Lets you access the object pointed to by the pointer

* :ada:`.all` is optional for

   - Access on a component of an array
   - Access on a component of a record

----------------------
Dereference Examples
----------------------

.. code:: Ada

   type R is record
     F1, F2 : Integer;
   end record;
   type A_Int is access Integer;
   type A_String is access all String;
   type A_R is access R;
   V_Int    : A_Int := new Integer;
   V_String : A_String := new String'("abc");
   V_R      : A_R := new R;

.. code:: Ada

   V_Int.all := 0;
   V_String.all := "cde";
   V_String (1) := 'z'; -- similar to V_String.all (1) := 'z';
   V_R.all := (0, 0);
   V_R.F1 := 1; -- similar to V_R.all.F1 := 1;

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

* As soon as you use them, you lose the safety of your pointers
* But sometimes, you have to do what you have to do ...

   - There's no simple way of doing it
   - Ada provides `Ada.Unchecked_Deallocation`
   - Has to be instantiated (it's a generic)
   - Must work on an object, reset to :ada:`null` afterwards

----------------------
Deallocation Example
----------------------

.. code:: Ada

   -- generic used to deallocate memory
   with Ada.Unchecked_Deallocation;
   procedure P is
      type An_Access is access A_Type;
      -- create instances of deallocation function
      -- (object type, access type)
      procedure Free is new Ada.Unchecked_Deallocation
        (A_Type, An_Access);
      V : An_Access := new A_Type;
   begin
      Free (V);
      -- V is now null
   end P;

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
