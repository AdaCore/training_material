==============
Introduction
==============

----------------------------
Discriminated Record Types
----------------------------

* :dfn:`Discriminated record` type

   + Different **objects** may have **different** components and/or different sizes
   + All objects **still** share the same type

* Similar to :C:`union` in C

   + But preserves **type checking**

      * Except in the case of an :ada:`Unchecked_Union` (seen later)

   + And object size **is related to** discriminant
    
* Aggregate assignment is allowed

   + Provided constraints are correct

---------------------------------
Defining a Discriminated Record
---------------------------------

* Record type with a :dfn:`discriminant` 

   * **Discriminant** controls behavior of the record
   * Part of record definition
   * Can be read as any other field

      * But can only be modified by object assignment (sometimes)

* Sample definitions (completions appear later in this module)

.. container:: latex_environment small

   .. code:: Ada

      type Employee_T (Kind : Category_T) is record ...
      type Mutable_T (Kind : Category_T := Employee) is record ...
      type Vstring (Last : Natural := 0) is record ...
      type C_Union_T (View : natural := 0) is record ...

