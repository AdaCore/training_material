==============
Introduction
==============

----------------------
Review: Access Types
----------------------

* Common access types in Ada are :dfn:`pool-specific`

  * Indicating we are allocating/deallocating from a pool of memory
  * Most often, this pool is the application heap

  .. code:: Ada

    type Integer_Access_T is access Integer;
    Pointer : Integer_Access_T := new Integer'(123);

* Pool-specific access types always reference items using some pool

  * Created via :ada:`new`
  * Destroyed via :ada:`Unchecked_Deallocation`

----------------------
General Access Types
----------------------

* Sometimes, we need an access type to point to an existing object

  * In C, that would be something like :cpp:`&anObject`

* To maintain memory safety, we cannot convert pool-specific access types

  * Each access type refers to it's own pool, so they are not compatible

* We need an access type that is a *view* of memory

  * So it doesn't care what pool it comes from

* We call these :dfn:`general access types`
