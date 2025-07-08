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
      Var := Var + 1;
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

   type Int is record
      Value : Integer;
   end record;
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

