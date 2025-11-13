==============
Introduction
==============

----------------------
General Access Types
----------------------

* Sometimes, we need an access type to point to an existing object

  * In C, that would be something like :cpp:`&anObject`

* To maintain memory safety, we cannot convert pool-specific access types

  * Each access type refers to its own pool, so they are not compatible

* We need an access type that is a *view* of memory

  * So it doesn't care what pool it comes from

* We call these :dfn:`general access types`

---------------------------
Why General Access Types?
---------------------------

* Less restrictive than pool-specific types

.. list-table::
   :header-rows: 1

  * - General
    - Pool-Specific

  * - Flexible
    - Restrictive

  * - Point to stack or heap
    - Must use allocation

  * - One global pool
    - Each type has its own pool
