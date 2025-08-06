=================
SPARK Libraries
=================

------------------------------
Pointers with Aliasing (1/2)
------------------------------

* SPARK Library defines two generics

  - :ada:`SPARK.Pointers.Pointers_With_Aliasing`
  - :ada:`SPARK.Pointers.Pointers_With_Aliasing_Separate_Memory`
  - Only generic parameter is any type :ada:`Object`

|

* Both allow aliasing pointers

  - Type :ada:`Pointer` is private

    + User code can copy such pointers freely
    + Ownership policy does not apply

  - All accesses through API check validity of pointer

------------------------------
Pointers with Aliasing (2/2)
------------------------------

* Shared API to create, free, access pointers

  .. code:: ada

     procedure Create (O : Object; P : out Pointer);
     function Deref (P : Pointer) return Object;
     procedure Assign (P : Pointer; O : Object);
     procedure Dealloc (P : in out Pointer);

* Version in :ada:`Pointers_With_Aliasing_Separate_Memory` adds parameter

  .. code:: ada

     Memory : in out Memory_Type

  - To handle separate groups of pointers in different memories

* Use of pointers with aliasing is *possible* but *costly*

  - Need to maintain validity of pointers at all times
  - Need to maintain separation of pointers at all times
  - This comes for free with the ownership policy

