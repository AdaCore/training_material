==============
Introduction
==============

---------------------
Access Types Design
---------------------

* An :dfn:`access type` is used to create objects by allocating space on the heap

* Access objects are associated with :dfn:`pools` of memory

  - Different allocation / deallocation policies
  - Each access type is unique - no conversion possible

* Access objects are **guaranteed** to always be meaningful

  - So long as :ada:`Unchecked_Deallocation` is not used
  - And when tied to a specific memory pool

**Each access type defines its own safe memory domain, managed by its pool.**

-------------------------------
Access Types Can Be Dangerous
-------------------------------

* Multiple memory issues

   - Leaks / corruptions

* Introduce potential random failures complicated to analyze
* Increase the complexity of the data structures
* May decrease the performance of the application

   - Dereferences are slightly more expensive than direct access
   - Allocations are a lot more expensive than stacking objects

* Ada avoids using accesses as much as possible

   - Arrays are not pointers
   - Parameters are implicitly passed by reference

.. tip:: Only use them when needed

---------------
Stack Vs Heap
---------------

.. code:: Ada

  I : Integer := 0;
  J : String := "Some Long String";

.. image:: items_on_stack.svg

.. code:: Ada

  I : Access_Int := new Integer'(0);
  J : Access_Str := new String'("Some Long String");

.. image:: stack_pointing_to_heap.svg
