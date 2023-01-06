**********************
Subprogram Contracts
**********************

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

-------------------------
Programming by Contract
-------------------------

* Pioneered by programming language Eiffel in the 80's

  - Since then adopted in Ada, .NET
  - Also being discussed for C++, Rust
  - Available as libraries for many languages

* The :dfn:`contract` of a subprogram defines:

  - What a caller guarantees to the subprogram (the precondition)
  - What the subprogram guarantees to its caller (the postcondition)

* A contract should include all the necessary information

  - Completes the API
  - Caller should not rely on implementation details
  - Typically parts of the contract are in English

--------------------
Contracts in SPARK
--------------------

* Preconditions and postconditions added in Ada 2012

  - Using the aspect syntax for :ada:`Pre` and :ada:`Post`
  - Already in GNAT since 2008 as pragmas

* Language support goes much beyond contracts-as-a-library

  - Ability to relate pre-state and post-state with attribute :ada:`Old`
  - Fine-grain control over execution

    .. code:: ada

       pragma Assertion_Policy (Pre => Check);
       pragma Assertion_Policy (Post => Ignore);

* :toolname:`GNATprove` analysis based on contracts

  - Precondition should be sufficient to prove subprogram itself
  - Postcondition should be sufficient to prove its callers
  - ...at all levels of software assurance beyond Bronze!

* SPARK contracts by cases, for callbacks, for OOP, etc.

=================
Frame Condition
=================

----------------------------
Quiz - Stating the Obvious
----------------------------

What is the problem with this postcondition?

.. code:: ada

   type Pair is record
      X, Y : Integer;
   end record;

   procedure Set_X (P : in out Pair; Value : Integer)
     with Post => P.X = Value;

.. container:: animate

   * The postcondition does not say that the value of :ada:`Y` is preserved!

   * As a result, nothing is known about :ada:`Y` after calling :ada:`Set_X`

     .. code:: ada

        P : Pair := Pair'(X => 1, Y => 2);
        P.Set_X (42);
        pragma Assert (P.Y = 2); -- unproved

---------------------------
Frame Condition - Records
---------------------------

* Simpler solution is to state what components are preserved

  .. code:: ada

     procedure Set_X (P : in out Pair; Value : Integer)
       with Post => P.X = Value and P.Y = P.Y'Old;

* Or with a delta aggregate

  .. code:: ada

     procedure Set_X (P : in out Pair; Value : Integer)
       with Post => P = (P'Old with delta X => Value);

* In both cases, value of :ada:`Y` is known to be preserved

--------------------------
Frame Condition - Arrays
--------------------------

* Use universal quantification to denote components preserved

  .. code:: ada

     procedure Swap_Table (T : in out Table; I, J : Index)
       with Post =>
         (for all K in T'Range =>
           (if K not in I | J then T (K) = T'Old (K)));

* Or with a delta aggregate

   .. code:: ada

     procedure Swap_Table (T : in out Table; I, J : Index)
       with Post =>
         T = (T'Old with delta I => T(J)'Old, J => T(I)'Old);

* In both cases, value of :ada:`T(K)` is known to be preserved for :ada:`K`
  different from :ada:`I` and :ada:`J`

------------------------------
Frame Condition - Conditions
------------------------------

* Any variable may be preserved conditionally

  - That applies also to scalar variables

  .. code:: ada

     procedure Zero_If (X : in out Integer; Cond : Boolean)
       with Post => (if Cond then X = 0);

* The preservation case needs to be explicited

  .. code:: ada

     procedure Zero_If (X : in out Integer; Cond : Boolean)
       with Post => (if Cond then X = 0 else X = X'Old);

* :dfn:`Frame condition` is **all** the parts of objects that may be preserved

  - Bounded by user-defined or generated data dependencies
  - Anything else needs to be stated explicitly

--------------------------------------------
Frame Condition - Bounds and Discriminants
--------------------------------------------

* Some parts of objects cannot be changed by a call

  - Array bounds
  - Discriminants of constrained records

* Special handling in :toolname:`GNATprove` to preserve them

  .. code:: ada

     type Rec (Disc : Boolean) is record ...

     procedure Change (T : in out Table; R : in out Rec)
       with Post =>
         T'First = T'First'Old         -- redundant
         and then T'Last = T'Last'Old  -- redundant
         and then R.Disc = R.Disc'Old; -- redundant

---------------------------------
Frame Condition - Private Types
---------------------------------

* Direct access to value or components not possible

* Simpler solution: define query functions

  - Hide access to value or components

  .. code:: ada

     type Pair is private;
     function Get_Y (P : Pair) return Integer;
     procedure Set_X (P : in out Pair; Value : Integer)
       with Post => P.Get_Y = P.Get_Y'Old;

* More comprehensive solution: define model functions

  - Create a visible model of the value

  .. code:: ada

     type Pair is private;
     type Pair_Model is record X, Y : Integer; end record;
     function Model (P : Pair) return Pair_Model;
     procedure Set_X (P : in out Pair; Value : Integer)
       with Post => P.Model = (P.Model'Old with delta X => Value);

-----------------------
Attribute :ada:`Old`
-----------------------

* Dynamic semantics is to make a copy at subprogram entry

  - Forbidden on limited types

* Evaluation for the copy may raise runtime errors

  - Not allowed by default inside *potentially unevaluated expressions*

    + Unless prefix is a variable

    .. code:: Ada

       procedure Extract (A : in out My_Array;
                          J : Integer;
                          V : out Value)
         with Post =>
           (if J in A'Range then V = A(J)'Old); -- Illegal

  - Use :ada:`pragma Unevaluated_Use_Of_Old (Allow)` to allow

    + :toolname:`GNATprove` checks that this is safe

-----------------------------------------
Special Cases for Attribute :ada:`Old`
-----------------------------------------

* Simple component access :ada:`X.C'Old` equivalent to :ada:`X'Old.C`

  - Although one may be more efficient at runtime

* Function call in the prefix of :ada:`Old` is evaluated at subprogram entry

  - Value of globals is the one at subprogram entry

  - Not the same as calling the function on parameters with :ada:`Old`

    .. code:: Ada

       function F (X : Integer) return Integer
         with Global => Glob;

       procedure P (X : in out Integer)
         with Post =>
           F (X'Old) = 0 and then
           F (X)'Old = 0;

====================
Contracts by Cases
====================

----------------------
Contract Cases (1/2)
----------------------

* Some contracts are best expressed by cases

  - Inspired by Parnas Tables

* SPARK defines aspect :ada:`Contract_Cases`

  - Syntax of named aggregate
  - Each case consists of a guard and a consequence

* Example from SPARK tutorial

  .. code:: ada

     Contract_Cases =>
       (A(1) = Val                              => ...
        Value_Found_In_Range (A, Val, 2, 10)    => ...
        (for all J in Arr'Range => A(J) /= Val) => ...

----------------------
Contract Cases (2/2)
----------------------

* :toolname:`GNATprove` checks that each case holds

  - When guard is enabled on entry, consequence holds on exit
  - Note: guards are evaluated *on entry*
  - Attributes :ada:`Old` and :ada:`Result` allowed in consequence

* :toolname:`GNATprove` checks that cases are disjoint and complete

  - All inputs allowed by the precondition are covered

* When enabled at runtime:

  - Runtime check that exactly one guard holds on entry
  - Runtime check that the corresponding consequence hold on exit

==========================
Contracts and Refinement
==========================

--------------------
What's Refinement?
--------------------

* :dfn:`Refinement` = relation between two representations

  - An :dfn:`abstract` representation
  - A :dfn:`concrete` representation

* Concrete behaviors are included in abstract behaviors

  - Analysis on the abstract representation
  - Findings are valid on the concrete one

* SPARK uses refinement

  - For analysis of callbacks
  - For analysis of dispatching calls in OOP

    - aka Liskov Substitution Principle (LSP)

* Generics do not follow refinement in SPARK

  - Reminder: instantiations are analyzed instead

------------------------
Contracts on Callbacks
------------------------

* Contracts can be defined on access-to-subprogram types

  - Only precondition and postcondition

  .. code:: ada

     type Update_Proc is access procedure (X : in out Natural)
     with
       Pre  => Precond (X),
       Post => Postcond (X'Old, X);

* :toolname:`GNATprove` checks refinement on actual subprograms

  .. code:: ada

     Callback : Update_Proc := Proc'Access;

  - Precondition of :ada:`Proc` should be weaker than :ada:`Precond(X)`
  - Postcondition of :ada:`Proc` should be stronger than
    :ada:`Postcond(X'Old, X)`
  - Data dependencies should be :ada:`null`

* :toolname:`GNATprove` uses contract of :ada:`Update_Proc` when
  :ada:`Callback` is called

-------------------
Contracts for OOP
-------------------

* Inherited contracts can be defined on dispatching subprograms

  .. code:: ada

     type Object is tagged record ...
     procedure Proc (X : in out Object) with
       Pre'Class  => Precond (X),
       Post'Class => Postcond (X'Old, X);

* :toolname:`GNATprove` checks refinement on overriding subprograms

  .. code:: ada

     type Derived is new Object with record ...
     procedure Proc (X : in out Derived) with ...

  - Precondition of :ada:`Proc` should be weaker than :ada:`Precond(X)`
  - Postcondition of :ada:`Proc` should be stronger than
    :ada:`Postcond(X'Old, X)`
  - Data dependencies should be the same

* :toolname:`GNATprove` uses contract of :ada:`Proc` in :ada:`Object` when
  :ada:`Proc` is called with static type :ada:`Object`

  - Dynamic type might be :ada:`Derived`

========================
Preventing Unsoundness
========================

--------------------
Quiz - Unsoundness
--------------------

What's wrong with the following contract?

.. code:: ada

   function Half (Value : Integer) return Integer
     with Post => Value = 2 * Half'Result;

.. container:: animate

   * The postcondition is false when :ada:`Value` is odd

   * :toolname:`GNATprove` generates an inconsistent axiom for :ada:`Half`

     - It says that any integer is equal to twice another integer
     - This can be used by provers to deduce :ada:`False`
     - Anything can be proved from :ada:`False`

       + As if the code was dead code

----------------------
Unfeasible Contracts
----------------------

* All contracts should be feasible

  - There exists a correct implementation
  - This includes absence of runtime errors

* Contract of :ada:`Double` also leads to unsoundness

  - The postcondition is false when :ada:`Value` is too large

  .. code:: ada

     function Double (Value : Integer) return Integer
       with Post => Double'Result = 2 * Value;

* :toolname:`GNATprove` implements defense in depth

  - Axiom only generated for functions (not procedures)
  - Function sandboxing adds a guard to the axiom

    + Unless switch :command:`--function-sandboxing=off` is used

  - Switch :command:`--proof-warnings` can detect inconsistencies
  - Proof of subprogram will detect contract unfeasibility

    + Except when subprogram does not terminate

---------------------------
Non-terminating Functions
---------------------------

What's wrong with the following code?

.. code:: ada

   function Half (Value : Integer) return Integer is
   begin
      if True then
         return Half (Value);
      else
         return 0;
      end if;
   end Half;

.. container:: animate

   * Function :ada:`Half` does not terminate

   * :toolname:`GNATprove` proves the postcondition of :ada:`Half`!

     - Because that program point is unreachable (dead code)

   * :toolname:`GNATprove` does not generate an axiom for :ada:`Half`

     - Because function may not terminate
     - :command:`info: function contract not available for proof`
     - Info message issued when using switch :command:`--info`

-----------------------
Terminating Functions
-----------------------

* All functions should terminate

  - Specific annotation to require proof of termination

  .. code:: ada

     Annotate => (GNATprove, Always_Return)

* Flow analysis proves termination in simple cases

  - No (mutually) recursive calls
  - Only bounded loops

* Proof used to prove termination in remaining cases

  - Based on subprogram variant for recursive subprograms
  - Based on loop variant for unbounded loops

---------------------
Subprogram Variants
---------------------

* Specifies measure on recursive calls

  - Either increases or decreases strictly

.. code:: ada

   function Half (Value : Integer) return Integer
     Subprogram_Variant =>
       (Increases => (if Value > 0 then -Value else Value)),
   is
   begin
      if Value in -1 .. 1 then
         return 0;
      elsif Value > 1 then
         return 1 + Half (Value - 2);
      else
         return -1 + Half (Value + 2);
      end if;
   end Half;

* More complex cases use lexicographic order

.. code:: ada

   Subprogram_Variant => (Decreases => Integer'Max(Value, 0),
                          Increases => Integer'Min(Value, 0)),

======
Quiz
======

------------------------
Quiz - Frame Condition
------------------------

Which statement is correct?

   A. :answer:`The frame condition is easily overlooked.`
   B. The frame condition is generated by :toolname:`GNATprove`.
   C. Delta aggregates are only used in frame conditions.
   D. Attribute :ada:`Old` is illegal after :ada:`and then` or :ada:`or else`.

.. container:: animate

   Explanations

   A. Correct
   B. Only part of the frame condition is generated.
   C. No, but they are particularly useful in frame conditions.
   D. Use pragma :ada:`Unevaluated_Use_Of_Old (Allow)`.

--------------------
Quiz - Unsoundness
--------------------

Which statement is correct?

   A. All functions terminate by definition in SPARK.
   B. An inconsistent axiom may be caused only by a non-terminating function.
   C. The only protection against unsoundness is reviews.
   D. :answer:`A proved terminating subprogram cannot lead to unsoundness.`

.. container:: animate

   Explanations

   A. No, recursion and infinite loops may cause non-termination.
   B. The contract may be unfeasible if the function is not proved.
   C. :toolname:`GNATprove` has multiple defenses against inconsistent axioms.
   D. Correct

=========
Summary
=========

----------------------
Subprogram Contracts
----------------------

* Functional contracts given by

  - The precondition with aspect :ada:`Pre`
  - The postcondition with aspect :ada:`Post`
  - The contract cases with aspect :ada:`Contract_Cases`

* Postcondition may be imprecise

  - In particular, frame condition might be missing
  - This may prevent proof of callers

* Function contracts may lead to unsoundness

  - If contract is unfeasible
  - If function does not terminate
  - Prove functions and their termination!
