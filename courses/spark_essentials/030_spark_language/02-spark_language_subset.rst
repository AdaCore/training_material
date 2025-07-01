=======================
SPARK Language Subset
=======================

----------------------------
Categories of Types in Ada
----------------------------

.. image:: types_tree.png

------------------------------
Categories of Types in SPARK
------------------------------

.. image:: types_tree.png

SPARK supports all the types in Ada, with some restrictions

---------------------
Assertions in SPARK
---------------------

* Assertions in Ada are just :ada:`Boolean` expressions

  - They can be executed
  - Thus they can raise run-time errors (to be checked in SPARK)

* Low-level assertions

  .. code:: ada

     pragma Assert (Idx in T'Range and then T (Idx) = 0);

* High-level assertions, aka specifications, aka :dfn:`contracts`

  .. code:: ada

     function Get (T : Table; Idx : Index) return Elem
       with Pre => Idx in T'Range and then T (Idx) = 0;

* Much more to come in later courses

-----------------------
Excluded Ada Features
-----------------------

* Backward :ada:`goto` statement

  - Can create loops, which require a specific treatment in formal verification

|

* Controlled types

  - Creates complex control flow with implicit calls

|

* Tasking features: :ada:`accept` statement (aka :dfn:`rendezvous`),
  :ada:`requeue` statement, :ada:`select` statement, etc

  - But features in Ravenscar and Jorvik profiles are supported

.. container:: speakernote

   And that's it! SPARK supports most features in Ada today.

----------------------
Support for Generics
----------------------

* Only **instances** of generics are analyzed

|

* Analysis of generics themselves would require:

  - Extending the SPARK language with new specifications

    + To name objects manipulated through calls to formal parameters
    + To add dependency contracts to formal subprogram parameters

  - More efforts from users to annotate programs

|

* **No restrictions** regarding use of generics

----------------------------
Support for OO Programming
----------------------------

* Root class and derived class (aka tagged types) must respect the
  :dfn:`Liskov Substitution Principle` (LSP)

  - Behavior of overriding subprogram must be a subset of the allowed behaviors
    of the overridden subprogram

    + Overridden subprogram is in root class
    + Overriding subprogram is in derived class

* Overriding subprogram puts less constraints on caller than overridden one

  - :dfn:`Precondition` must be weaker in overriding subprogram

* Overriding subprogram gives more guarantees to caller than overridden one

  - :dfn:`Postcondition` must be stronger in overriding subprogram

* Overriding subprogram cannot access more global variables than overridden one

-------------------------
Support for Concurrency
-------------------------

* Ravenscar and Jorvik profiles of Ada are **supported**
* Tasks and protected objects must be defined at **library level**
* Tasks can only communicate through :dfn:`synchronized objects`

  - Protected objects
  - Atomic objects

* This ensures absence of data races (aka race conditions)

  - One task writes an object while another task reads it
  - Two tasks write the object at the same time

* This is also a benefit for programs on a single core!

  - Concurrency :math:`\neq` parallelism

