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
Stack vs Heap
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

