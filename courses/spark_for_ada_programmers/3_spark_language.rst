****************
SPARK Language
****************

.. container:: PRELUDE BEGIN

.. container:: PRELUDE ROLES

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

.. role:: rust(code)
    :language: Rust

.. container:: PRELUDE SYMBOLS

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`
.. |checkmark| replace:: :math:`\checkmark`

.. container:: PRELUDE REQUIRES

.. container:: PRELUDE PROVIDES

.. container:: PRELUDE END

==============
Introduction
==============

------------------------
Design Goals for SPARK
------------------------

* Support formal analysis that is

  - Deep - it tells you something **useful**
  - Sound - it has **no** missing alarms
  - Precise - it has **few** false alarms
  - Fast - it can run as part of development
  - Modular - it analyzes modules in **parallel**
  - Constructive - it works on **incomplete programs**

* Combine tool automation and user interaction

  - Automate as much as possible
  - Rely on the user to provide essential code annotations

* Combine execution and proof of specifications
* Support the largest possible subset of Ada 2022

---------------------
Excluding Ambiguity
---------------------

* Soundness requires that program semantics are **clear**
* Easiest way is to avoid **language** ambiguities:

  - No :dfn:`erroneous behavior` from Ada Reference Manual

    - Cases where error can't be detected by the compiler or at run-time: e.g.
      dereference a pointer after it was deallocated

  - No :dfn:`unspecified` features from Ada Reference Manual

    - Cases where the compiler makes a choice: e.g. order of evaluation of
      parameters in a call

  - Limit :dfn:`implementation defined` features from Ada Reference Manual

    - Cases where the choice of the compiler is documented: e.g. size of
      standard integer types

    - Analyzer should make the same choices as the compiler

* Also facilitates **portability** across platforms and compilers!

------------------------
SPARK Reference Manual
------------------------

* Precise definition of the SPARK subset
* Builds on the Ada Reference Manual

  - Follows the **same section numbering**
  - Has similar subsections:

    + **Syntax**
    + **Name Resolution Rules**
    + **Legality Rules**
    + **Static Semantics**
    + **Dynamic Semantics**
    + **Verification Rules** *(specific to SPARK RM)*
    + **Examples**

:url:`https://docs.adacore.com/live/wave/spark2014/html/spark2014_rm/packages.html`

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
  - Thus they can raise runtime errors (to be checked in SPARK)

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

=======================
Language Restrictions
=======================

----------------------------
Main Language Restrictions
----------------------------

* Regular functions **without side-effects**

  - Thus expressions are also without side-effects
  - Aspect :ada:`Side_Effects` to signal function with side-effects

* Memory **ownership** policy (like in Rust)
* Absence of interferences

  - No problematic aliasing between variables

* Termination of subprograms

  - Functions must **always** terminate normally

* OO programming must respect Liskov Substitution Principle
* Concurrency must support Ravenscar or Jorvik profile

.. container:: speakernote

   There are more minor restrictions on the user of specific features, like
   some expressions which cannot mention variables.

--------------------------------
Functions Without Side-Effects
--------------------------------

* :dfn:`Side-effects` of a function are:

  - Writing to a global variable
  - Writing to an :ada:`out` or :ada:`in out` parameter
  - Reading a volatile variable
  - Raising an exception
  - Not terminating

|

* But :dfn:`volatile functions` can read a volatile variable

  - Details discussed in the course on SPARK Boundary

|

* Only :dfn:`functions with side-effects` can have side-effects

  - Signaled with aspect :ada:`Side_Effects`
  - Restricted to appear only as right-hand side of assignments

----------------------------
Side-Effects and Ambiguity
----------------------------

* If function :ada:`Fun` writes to global variable :ada:`Var`, what is the
  value of the expression :ada:`Fun = Var`?

  - :ada:`Var` may be evaluated before the call to :ada:`Fun`
  - ...or after the call to :ada:`Fun`
  - Thus leading to an ambiguity

.. code:: Ada

   Var : Integer := 0;
   function Fun return Integer is
   begin
      Var := Var + 1
      return Var;
   end Fun;
   pragma Assert (Fun = Var); -- Ambiguous evaluation

* Same with :ada:`Fun` writing to an :ada:`out` or :ada:`in out` parameter

--------------------------------------------
Benefits of Functions Without Side-Effects
--------------------------------------------

* Expressions have no side-effects

  - **Unambiguous** evaluation of expressions
  - Simplifies both flow analysis and proof

|

* Specifications and assertions have no side-effects

  - As specifications and assertions are expressions

|

* SPARK functions are **mathematical functions** from inputs to a result

  - Interpreted as such in proof

--------------------------
Absence of Interferences
--------------------------

* :dfn:`Interferences` between names :ada:`A` and :ada:`B` when:

  - :ada:`A` and :ada:`B` designate the **same object** (:dfn:`aliasing`)
  - and the code writes to :ada:`A`, then reads :ada:`B`
  - or the code writes to :ada:`A` and to :ada:`B`

|

* Interferences are caused by passing parameters

  - Parameter and global variable may designate the same object
  - Two parameters may designate the same object

|

* Thus no interferences on function calls!

-----------------------------------
Interferences and Ambiguity (1/2)
-----------------------------------

* If procedure :ada:`Proc` writes to parameter :ada:`A` then to parameter
  :ada:`B`, what is the value of `Var` after the call :ada:`Proc (Var, Var)`?

  - if :ada:`A` and :ada:`B` are passed by reference: the value of :ada:`B`
  - if :ada:`A` and :ada:`B` are passed by copy: the value of :ada:`A` or
    :ada:`B`, depending on which one is copied back last

  - Thus leading to an ambiguity

.. code:: Ada

   Var : Integer := 0;
   procedure Proc (A, B : out Integer) is
   begin
      A := 0;
      B := 1;
   end Proc;
   Proc (Var, Var); -- Ambiguous call

* Actually, Ada forbids this simple case and GNAT rejects it

  - But problem remains with :ada:`Table(Var)` instead of :ada:`Var`

-----------------------------------
Interferences and Ambiguity (2/2)
-----------------------------------

* If procedure :ada:`Proc` writes to parameter :ada:`A` then reads global
  variable :ada:`Var`, what is the value read in a call to :ada:`Proc (Var)`?

  - if :ada:`A` is passed by reference: the value written to :ada:`A`
  - if :ada:`A` is passed by copy: the initial value of :ada:`Var`
  - Thus leading to an ambiguity

.. code:: Ada

   type Int is record Value : Integer; end record;
   Var : Int := (Value => 0);
   procedure Proc (A : out Int) is
   begin
      A := (Value => 1);
      pragma Assert (Var = A); -- Ambiguous
   end Proc;
   Proc (Var);

* Ada cannot forbid and GNAT cannot detect this case

.. container:: speakernote

   Ask the audience in which case the assertion always succeeds!
   (answer: when A is passed by reference)

--------------------------------------
Benefits of Absence of Interferences
--------------------------------------

* No hidden changes to an object :ada:`A` through another unrelated name

  - **Simplifies** both flow analysis and proof

|

* No need for users to add specifications about separation

  - Between parameters and global variables
  - Between parameters themselves
  - Between parts of objects (one could be a part of another)

|

* Program behavior does not depend on parameter-passing mechanism

  - This improves **portability** across platforms and compilers!

====================
Migrating to SPARK
====================

-----------------------------
Migrating From Ada to SPARK
-----------------------------

* Analyzing the Ada code will point to SPARK violations
* First goal is to reach **Stone level**: Valid SPARK
* Violation: functions with side-effects

  - Fix: add aspect :ada:`Side_Effects` to functions, move calls to assignments

* Violation: pointers do not respect ownership

  - Fix: change types and code to respect ownership

* Violation: illegal use of (volatile) variables inside expressions or
  functions

  - Fix: introduce temporaries, mark functions as volatile

* Define a SPARK interface for a unit in Ada

  - Details discussed in the course on SPARK Boundary

----------------------------
Adoption Guidance Document
----------------------------

.. container:: columns

 .. container:: column

    * Based on adoption experience
    * Proposes adoption levels
    * For every level, presents:

       - Benefits, impact on process, costs, and limitations
       - Setup and tool **usage**
       - **Messages** issued by the tool
       - **Remediation** solutions

 .. container:: column

    .. image:: thales_adoption_manual.png
       :width: 100%

---------------------------
Migrating From C to SPARK
---------------------------

* Same recommendations as when migrating from C to Ada
* Even more important to use appropriate types

  - private types as much as possible (e.g. private type for flags with
    constants and boolean operator instead of modular type)

  - enumerations instead of :code:`int`
  - ranges on scalar types
  - non-null access types
  - type predicates

* Special attention on the use of pointers

  - C uses pointers **everywhere**
  - Better to use parameter modes :ada:`out` and :ada:`in out` and array
    types in Ada

  - Choose between **different access types** in SPARK, with different semantics

    + Details discussed in the course on Pointer Programs

=========
Summary
=========

----------------
SPARK Language
----------------

* SPARK was designed **for formal analysis**
* **Soundness** is key!

  - No language ambiguities
  - Hence regular functions without side-effects
  - Hence absence of interferences

* Still, SPARK subset is most of Ada 2022

  - All categories of types
  - OO programming with LSP
  - Concurrency with Ravenscar and Jorvik
  - Pointer programs with ownership

* Recommendations for migration from Ada or C
