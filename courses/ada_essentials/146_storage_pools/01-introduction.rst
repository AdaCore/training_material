==============
Introduction
==============

----------------------------------------
Controlling Where and How Objects Live
----------------------------------------

* :dfn:`Storage Pool`

  * Region of memory managed as a unit
  * Objects/blocks are allocated (and released) from the storage pool

* Many times, we don't want to worry about memory management

  * Just use :ada:`new` and :ada:`Unchecked_Deallocation`
  * Affect the system heap, and we have no control

* Sometimes we need better performance or control

  * Define mechanisms to allocate / free / query memory usage
  * Allows checking of memory status as part of processing

----------------------
Storage Pools in Ada
----------------------

:ada:`System.Storage_Pools`

* Allows specification of your own block of memory to maintain
* :ada:`new` uses this block for allocations
* Mechanisms to allocate, deallocate, and query
* Each access type can have its own storage pool
