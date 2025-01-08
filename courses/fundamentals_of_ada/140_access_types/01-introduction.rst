==============
Introduction
==============

---------------------
Access Types Design
---------------------

* Memory-addressed objects are called :dfn:`access types`
* Objects are associated to :dfn:`pools` of memory

  - With different allocation / deallocation policies

* Access objects are **guaranteed** to always be meaningful

  - In the absence of :ada:`Unchecked_Deallocation`
  - And if pool-specific

.. container:: columns

 .. container:: column

  * Ada

  .. code:: Ada

     type Integer_Pool_Access
       is access Integer;
     P_A : Integer_Pool_Access
       := new Integer;

     type Integer_General_Access
       is access all Integer;
     G : aliased Integer;
     G_A : Integer_General_Access := G'Access;

 .. container:: column

  * C++

  .. code:: C++

     int * P_C = malloc (sizeof (int));
     int * P_CPP = new int;
     int * G_C = &Some_Int;
.

-----------------------------------------
Access Types - General vs Pool-Specific
-----------------------------------------

.. container:: columns

  .. container:: column

    **General Access Types**

      * Point to any object of designated type
      * Useful for creating aliases to existing objects
      * Point to existing object via :ada:`'Access` **or** created by :ada:`new`
      * No automatic memory management

  .. container:: column

    **Pool-Specific Access Types**

      * Tightly coupled to dynamically allocated objects
      * Used with Ada's controlled memory management (pools)
      * Can only point to object created by :ada:`new`
      * Memory management tied to specific storage pool

-------------------------------
Access Types Can Be Dangerous
-------------------------------

* Multiple memory issues

   - Leaks / corruptions

* Introduces potential random failures complicated to analyze
* Increase the complexity of the data structures
* May decrease the performances of the application

   - Dereferences are slightly more expensive than direct access
   - Allocations are a lot more expensive than stacking objects

* Ada avoids using accesses as much as possible

   - Arrays are not pointers
   - Parameters are implicitly passed by reference

* Only use them when needed

---------------
Stack Vs Heap
---------------

.. code:: Ada

  I : Integer := 0;
  J : String := "Some Long String";

.. image:: items_on_stack.png
   :width: 50%

.. code:: Ada

  I : Access_Int := new Integer'(0);
  J : Access_Str := new String'("Some Long String");

.. image:: stack_pointing_to_heap.png
   :width: 50%

