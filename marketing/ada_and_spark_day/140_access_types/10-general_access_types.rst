======================
General Access Types
======================

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

----------------------------
Using General Access Types
----------------------------

* To differentiate the access types, add the keyword :ada:`all`

  .. code:: Ada

    type Gen_Acc_T is access all Integer;
    type Gen_Acc_2 is access all Integer;
    Allocated : Gen_Acc_T := new Integer'(123);

* Remember, we cannot compare pool-specific access types

  * Because each access type deals with its own memory pool

  .. code:: Ada
    :number-lines: 2

    type Acc_T is access Integer;
    type Acc_2 is access Integer;
    Acc1 : Acc_T := new Integer;
    Acc2 : Acc_2 := Acc_2 (Acc1);

* But general access types don't refer to a specific pool

  .. code:: Ada

    Alloc2 : Gen_Acc_2 := Gen_Acc_2 (Allocated);

-----------------------
Referencing the Stack
-----------------------

* By default, stack-allocated objects cannot be referenced

  * May be optimized into a register by the compiler

* :ada:`aliased` declares an object to be referenceable through an access value

  .. code:: Ada

     Object : aliased Integer;

* :ada:`'Access` attribute gives a reference to the object

  .. code:: Ada

     Access_Object : Gen_Access_T := Object'Access;
