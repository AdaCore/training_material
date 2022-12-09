****************
SPARK Language
****************

..
    Coding language

.. role:: ada(code)
    :language: Ada

..
    Math symbols

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

------------------------
Design Goals for SPARK
------------------------

* Support formal analysis that is

  - Deep - it tells you something useful
  - Sound - it has no missing alarms
  - Precise - it has few false alarms
  - Fast - it can run as part of development
  - Modular - it does not require the complete program

* Combine tool automation and user interaction

  - Automate as much as possible
  - Rely on the user to provide essential code annotations

* Combine execution and proof of specifications

* Support the largest possible subset of Ada

---------------------
Excluding Ambiguity
---------------------

* Soundness requires that program semantics are clear

* Easiest way is to avoid language ambiguities:

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

* Also facilitates portability across platforms and compilers!

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

.. container:: speakernote

   SPARK supports all the types in Ada, but with restrictions.

-----------------------
Excluded Ada Features
-----------------------

* Backward :code:`goto` statement

  - Can create loops, which require a specific treatment in formal verification

* Controlled types

  - Creates complex control flow with implicit calls

* Exception handlers

  - Creates complex control flow across calls
  - Raising exceptions is allowed

* Tasking features: :code:`accept` statement (aka :dfn:`rendezvous`),
  :code:`requeue` statement, :code:`select` statement, etc

  - But features in Ravenscar and Jorvik profiles are supported

.. container:: speakernote

   And that's it! SPARK supports most features in Ada today.

----------------------
Support for Generics
----------------------

* Only instances of generics are analyzed

* Analysis of generics themselves would require:

  - Extending the SPARK language with new specifications

    + To name objects manipulated through calls to formal parameters

    + To add specifications to formal subprogram parameters

  - More efforts from users to annotate programs

* No restrictions regarding use of generics

----------------------------
Support for OO Programming
----------------------------

* Root class and derived class (aka tagged types) must respect the Liskov
  Substitution Principle (LSP)

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

* Ravenscar and Jorvik profiles of Ada are supported

* Tasks and protected objects must be defined at library level

* Tasks can only communicate through :dfn:`synchronized objects`

  - Protected objects

  - Atomic objects

* This ensures absence of data races (aka race conditions)

  - One task writes an object while another task reads it

  - Two tasks write the object at the same time

* This is also a benefit for programs on a single core!

  - Concurrency :math:`neq` parallelism

=======================
Language Restrictions
=======================

----------------------------
Main Language Restrictions
----------------------------

* Functions without side-effects

  - Thus expressions are also without side-effects

* Data initialization policy

  - All inputs and outputs are completely initialized

* Memory ownership policy (like in Rust)

* Absence of interferences

* Termination of subprograms

  - Functions must always terminate normally

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

  - Writing to an :code:`out` or :code:`in out` parameter

  - Reading a volatile variable

* But :dfn:`volatile functions` can read a volatile variable

  - Details discussed in the course on SPARK Boundary

----------------------------
Side-Effects and Ambiguity
----------------------------

* If function :code:`Fun` writes to global variable :code:`Var`, what is the
  value of the expression :code:`Fun = Var`?

  - :code:`Var` may be evaluated before the call to :code:`Fun`

  - ...or after the call to :code:`Fun`

  - Thus leading to an ambiguity

.. code:: Ada

   Var : Integer := 0;
   function Fun return Integer is
   begin
      Var := Var + 1
      return Var;
   end Fun;

   pragma Assert (Fun = Var); -- Ambiguous evaluation

* Same with :code:`Fun` writing to an :code:`out` or :code:`in out` parameter

--------------------------------------------
Benefits of Functions Without Side-Effects
--------------------------------------------

* Expressions have no side-effects

  - Unambiguous evaluation of expressions

  - Simplifies both flow analysis and proof

* Specifications and assertions have no side-effects

  - As specifications and assertions are expressions

* SPARK functions are mathematical functions from inputs to a result

  - Interpreted as such in proof

--------------------------
Absence of Interferences
--------------------------

* :dfn:`Interferences` between names :code:`A` and :code:`B` when:

  - :code:`A` and :code:`B` designate the same object

  - and the code writes to :code:`A`, then reads :code:`B`

  - or the code writes to :code:`A` and to :code:`B`

* Interferences are caused by passing parameters

  - Parameter and global variable may designate the same object

  - Two parameters may designate the same object

* Thus no interferences on function calls!

-----------------------------------
Interferences and Ambiguity (1/2)
-----------------------------------

* If procedure :code:`Proc` writes to parameter :code:`A` then to parameter
  :code:`B`, what is the value of `Var` after the call :code:`Proc (Var, Var)`?

  - if :code:`A` and :code:`B` are passed by reference: the value of :code:`B`

  - if :code:`A` and :code:`B` are passed by copy: the value of :code:`A` or
    :code:`B`, depending on which one is copied back last

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

  - But problem remains with :code:`Table(Var)` instead of :code:`Var`

-----------------------------------
Interferences and Ambiguity (2/2)
-----------------------------------

* If procedure :code:`Proc` writes to parameter :code:`A` then reads global
  variable :code:`Var`, what is the value read in a call to :code:`Proc (Var)`?

  - if :code:`A` is passed by reference: the value written to :code:`A`

  - if :code:`A` is passed by copy: the initial value of :code:`Var`

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

* No hidden changes to an object :code:`A` through another unrelated name

  - Simplifies both flow analysis and proof

* No need for users to add specifications about separation

  - Between parameters and global variables

  - Between parameters themselves

  - Between parts of objects (one could be a part of another)

* Program behavior does not depend on parameter-passing mechanism

  - This improves portability across platforms and compilers!

====================
Migrating to SPARK
====================

-----------------------------
Migrating from Ada to SPARK
-----------------------------

* Analyzing the Ada code will point to SPARK violations

* First goal is to reach Stone level

* Violation: functions with side-effects

  - Fix: transform function into procedure

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
       - Setup and tool usage
       - Messages issued by the tool
       - Remediation solutions

 .. container:: column

    .. image:: thales_adoption_manual.png
       :width: 100%

---------------------------
Migrating from C to SPARK
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

  - C uses pointers everywhere

  - Better to use parameter modes :code:`out` and :code:`in out` and array
    types in Ada

  - Choose between different access types in SPARK, with different semantics

    + Details discussed in the course on Pointer Programs

======
Labs
======

=========
Summary
=========

----------------
SPARK Language
----------------

* SPARK was designed for formal analysis

* Soundness is key!

  - No language ambiguities

  - Hence functions without side-effects

  - Hence absence of interferences

* Still, SPARK subset is most of Ada

  - All categories of types

  - OO programming with LSP

  - Concurrency with Ravenscar and Jorvik

  - Pointer programs with ownership

* Recommendations for migration from Ada or C
