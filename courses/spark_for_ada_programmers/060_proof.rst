*******
Proof
*******

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

----------------
What Is Proof?
----------------

* **Second** static analysis performed by :toolname:`GNATprove`

   - Depends on successful flow analysis

* Models the **computation** in a subprogram
* Models **assertions** in a subprogram
* Performs checks and detects **violations**

   - Generates **logical formulas**

     + aka Verification Conditions (VC)
     + aka Proof Obligations (PO)

   - Automatic provers check that the VC is valid (always true)
   - If not, a check message is emitted

---------------
Hoare Triples
---------------

* **Hoare triples** (1969) used to reason about program correctness

   - With pre- and postconditions

* Syntax: ``{P} S {Q}``

   - ``S`` is a program
   - ``P`` and ``Q`` are **predicates**
   - ``P`` is the **precondition**
   - ``Q`` is the **postcondition**

* Meaning of ``{P} S {Q}`` triple:

   - If we start in a state where ``P`` is true and execute ``S``, then ``S``
     will terminate in a state where ``Q`` is true.

----------------------
Quiz - Hoare Triples
----------------------

Which one of these is **invalid**?

   A. ``{ X >= 3 } Y := X – 1 { Y >= 0 }``
   B. ``{ X >= 3 } Y := X – 1 { Y = X – 1 }``
   C. ``{ False } Y := X – 1 { Y = X }``
   D. :answermono:`{ X >= 3 } Y := X – 1 { Y >= 3 }`
   E. ``{ X >= 3 } Y := X – 1 { True }``

.. container:: animate

   Explanations

   A. :ada:`Y >= 2` entails :ada:`Y >= 0`
   B. This is true independent of the precondition.
   C. This is true independent of the postcondition.
   D. **Invalid**: :ada:`Y >= 2` does not entail :ada:`Y >= 3`
   E. This is true independent of the precondition.

-----------------------------------------
VC Generation - Strongest Postcondition
-----------------------------------------

* VC are generated using a *Strongest Postcondition Calculus*
* The strongest postcondition ``Q`` for a program ``S`` and a precondition
  ``P`` is such that:

  - ``{P} S {Q}`` is a valid Hoare triple
  - For every valid Hoare triple ``{P} S {Q'}``, ``Q`` is **stronger** than ``Q'``,
    i.e. ``Q`` implies ``Q'``

* The strongest postcondition **summarizes** what is known at any program point
* The strongest postcondition is computed through a *predicate transformer*

  - Information is **propagated** from the precondition
  - VCs are generated each time a **check** is encountered

--------------------------------
Quiz - Strongest Postcondition
--------------------------------

Which one of these has a **Strongest Postcondition**?

   A. ``{ X >= 3 } Y := X – 1 { Y >= 0 }``
   B. ``{ X >= 3 } Y := X – 1 { Y = X – 1 }``
   C. ``{ X >= 3 } Y := X – 1 { Y >= 2 }``
   D. :answermono:`{ X >= 3 } Y := X – 1 { Y = X – 1 and Y >= 2 }`
   E. :answermono:`{ X >= 3 } Y := X – 1 { Y = X – 1 and X >= 3 }`

.. container:: animate

   Explanations

   A. Information about :ada:`X` is lost.
   B. Information about :ada:`X` is lost.
   C. Information about :ada:`X` is lost.
   D. Correct
   E. Correct (equivalent to answer D)

=======
Proof
=======

----------------------
Functional Contracts
----------------------

* Precondition introduced by aspect :ada:`Pre`

  - Boolean expression stating **constraint on the caller**
  - Constraint on the value of inputs

* Postcondition introduced by aspect :ada:`Post`

  - Boolean expression stating **constraint on the subprogram**
  - Constraint on the value of inputs and outputs

* On the first declaration of a subprogram

  - This can be a spec or a body

* Optional, default is :ada:`True`

  - Precondition: subprogram can be called in any context
  - Postcondition: subprogram gives no information on its behavior

* Special attributes in postconditions

  - :ada:`X'Old` denotes the input value of :ada:`X`
  - :ada:`F'Result` denotes the result of function :ada:`F`

-----------------------------
Silver/Gold/Platinum Levels
-----------------------------

* Check absence of runtime errors (AoRTE)
* Check that assertions are always true
* Check that code respects functional contracts

  .. code:: Ada

     procedure Swap (X, Y : in out Integer)
     with
       Post => X = Y'Old and Y = X'Old; -- Wrong

     procedure Swap (X, Y : in out Integer) is
     begin
        Temp := Y;
        X := Y;
        Y := Temp;
     end Swap;

* Warn on dead code with switch :command:`--proof-warnings`

  - More powerful than the detection by flow analysis

-------------------------------
Run-Time Errors Are Pervasive
-------------------------------

.. container:: columns

 .. container:: column

    * A simple assignment statement

      .. code:: Ada

         A (I + J) := P / Q;

    * Which are the possible run-time errors for this example?

 .. container:: column

    * ``I+J`` might overflow the base type of the index range's subtype
    * ``I+J`` might be outside the index range's subtype
    * ``P/Q`` might overflow the base type of the component type
    * ``P/Q`` might be outside the component subtype
    * ``Q`` might be zero

-------------------------------
Categories of Run-Time Errors
-------------------------------

* Divide by zero

  - Arithmetic operations: division, :ada:`mod`, :ada:`rem`

* Index check

  - Read/write access in an array

* Overflow check

  - Most arithmetic operations
  - Checking that result is within bounds of the machine integer or float

* Range check

  - Type conversion, type qualification, assignment
  - Checking that the value satisfies range constraint of type

* Discriminant check

  - Read/write access in a discriminated record

* Length check

  - Assignment of an array or string

* Checks on pointer programs - Details in the course on Pointer Programs

-----------------------------------------
Quiz - Special Cases of Run-Time Errors
-----------------------------------------

Consider the following declarations:

.. code:: ada

   type Table is array (Natural range <>) of Integer;
   type Rec (Disc : Boolean) is record ...
   T : Table := ...;
   R : Rec := ...;
   X : Integer;

Which of the following *cannot* cause a runtime error:

   A. ``X := T (T'First)``
   B. ``X := X / (-1);``
   C. ``X := abs X;``
   D. ``X := T'Length;``
   E. ``R := (Disc => True, ...);``

.. container:: animate

   Explanations: **all** of then can cause a runtime error!

   A. Index check fails if :ada:`T` is empty.
   B. Overflow check fails if :ada:`X = Integer'First`
   C. Overflow check fails if :ada:`X = Integer'First`
   D. Range check fails if :ada:`T'Range` is :ada:`Natural`
   E. Discriminant check fails if :ada:`R.Disc /= True`

--------------------------
Categories of Assertions
--------------------------

* Pragma :ada:`Assert` and similar (:ada:`Assert_And_Cut`, :ada:`Assume`)

  - AoRTE is also proved for its expression

* Precondition on call

  - AoRTE is also proved for **any** calling context
  - This may require **guarding** the precondition

  .. code:: ada

     procedure Update (T : in out Table; X : Index; V : Value)
       with Pre => T (X) /= V; -- Index check might fail
       with Pre => X in T'Range and T (X) /= V; -- Same
       with Pre => X in T'Range and then T (X) /= V; -- OK

* Postcondition on subprogram

  - AoRTE is proved in the context of the subprogram **body**
  - Still better to include info for AoRTE in **caller**

  .. code:: ada

     procedure Find (T : Table; X : out Index; V : Value)
       with Post => T (X) = V; -- Not known that X in T'Range
       with Post => X in T'Range and then T (X) = V; -- OK

------------------------------
Levels of Software Assurance
------------------------------

* Silver level

  - Goal is **absence** of runtime errors
  - Functional contracts added to support that goal

    + Typically a few preconditions only

  .. code:: ada

     procedure Update (T : in out Table; X : Index; V : Value)
       with Pre => X in T'Range;

* Gold level

  - Builds on the Silver level
  - Functional contracts added to **express desired properties**

  .. code:: ada

     procedure Update (T : in out Table; X : Index; V : Value)
       with Pre  => X in T'Range,
            Post => T (X) = V;

* Platinum level

  - Same as Gold level
  - But the **full** functional specification is expressed as contracts

  .. code:: ada

     procedure Update (T : in out Table; X : Index; V : Value)
       with Pre  => X in T'Range,
            Post => T = (T'Old with delta X => V);

---------------
Preconditions
---------------

* Default precondition of :ada:`True` may **not** be sufficient

  .. code:: ada

     procedure Increment (X : in out Integer) is
     begin
        X := X + 1; -- Overflow check might fail
     end Increment;

* Precondition constrains **input context**

  .. code:: ada

     procedure Increment (X : in out Integer)
     with
       Pre => X < Integer'Last
     begin
        X := X + 1; -- Overflow check proved
     end Increment;

----------------
Postconditions
----------------

* Default postcondition of :ada:`True` may **not** be sufficient

  .. code:: ada

     procedure Add2 (X : in out Integer)
     with
       Pre => X < Integer'Last - 1
     is
     begin
        Increment (X);
        Increment (X); -- Precondition might fail
     end Add2;

* Postcondition constrains **output context**

  .. code:: ada

     procedure Increment (X : in out Integer)
     with
       Pre  => X < Integer'Last,
       Post => X = X'Old + 1;

     procedure Add2 (X : in out Integer)
     with
       Pre => X < Integer'Last - 1
     is
     begin
        Increment (X);
        Increment (X); -- Precondition proved
     end Add2;

------------------------------------------
Contextual Analysis of Local Subprograms
------------------------------------------

* Local subprograms without contracts are *inlined* in proof

  - Local: declared inside private part or body
  - Without contracts: no :ada:`Global`, :ada:`Pre`, :ada:`Post`, etc.
  - Additional conditions, details in the SPARK User's Guide

|

* Benefit: no need to add a contract

|

* Possible cost: proof of caller may become more complex

  - Add explicit contract like :ada:`Pre => True` to disable inlining of a
    subprogram
  - Use switch :command:`--no-inlining` to disable this feature globally
  - Use switch :command:`--info` to get more information about inlined
    subprograms

-------------------------
Overflow Checking (1/2)
-------------------------

* Remember: assertions might fail overflow checks

  .. code:: ada

     procedure Saturate_Add (X, Y : Natural; Z : out Natural)
       with Post => Z = Integer'Min (X + Y, Natural'Last);

* Sometimes property can be expressed to avoid overflows

  .. code:: ada

     procedure Saturate_Add (X, Y : Natural; Z : out Natural)
       with Post => Z =
         (if X <= Natural'Last - Y then X + Y else Natural'Last);

* Or a larger integer type can be used for computations

  .. code:: ada

     subtype LI is Long_Integer;

     procedure Saturate_Add (X, Y : Natural; Z : out Natural)
       with Post => LI(Z) =
         LI'Min (LI(X) + LI(Y), LI(Natural'Last));

-------------------------
Overflow Checking (2/2)
-------------------------

* Alternative: use a library of big integers

  - From SPARK Library :filename:`SPARK.Big_Integers`
  - Or Ada stdlib: :filename:`Ada.Numerics.Big_Numbers.Big_Integers`

  .. code:: Ada

     function Big (Arg : Integer) return Big_Integer is
       (To_Big_Integer (Arg)) with Ghost;
     procedure Saturate_Add (X, Y : Natural; Z : out Natural)
       with Post => Z =
         (if Big (X) + Big (Y) <= Big (Natural'Last)
          then X + Y else Natural'Last);

* Or use compiler switch :command:`-gnato13` to use big integers in all assertions

  - Implicit use
  - Should be used also when compiling assertions
  - Only applies to arithmetic operations (not :ada:`Integer'Min`)

  .. code:: ada

     procedure Saturate_Add (X, Y : Natural; Z : out Natural)
       with Post => Z =
         (if X + Y <= Natural'Last then X + Y else Natural'Last);

======================
Limitations of Proof
======================

---------------------------
Functional Specifications
---------------------------

* **Non-functional** specifications **cannot** be expressed as contracts

   - Time or space complexity
   - Timing properties for scheduling
   - Call sequences

* But **automatons** can be encoded as contracts

   - Being in a given state is a functional property
   - Can use normal queries

     + e.g. contracts on :filename:`Ada.Text_IO` use :ada:`Is_Open`

   - Or ghost imported functions that cannot be executed

     + When query cannot be expressed in the code

-----------------------------------------------
Limitations of Automatic Provers - Arithmetic
-----------------------------------------------

* Provers struggle with non-linear arithmetic

   - Use of multiplication, division, :ada:`mod`, :ada:`rem`
   - e.g. monotonicity of division on positive values
   - Solution: use **lemmas** from the SPARK Lemma Library

* Provers struggle with mixed arithmetic

   - Mix of signed and modular integers
   - Mix of integers and floats
   - Solution: define lemmas for **elementary properties**

------------------------------------------------
Limitations of Automatic Provers - Quantifiers
------------------------------------------------

* Quantified expressions express property over a **collection**

   - Universal: :ada:`(for all I in T'Range => T(I) /= 0)`
   - Existential: :ada:`(for some I in T'Range => T(I) /= 0)`

* Provers struggle with **existential**

   - Need to exhibit a :dfn:`witness` that satisfies the property
   - Solution: define a function that computes the witness

* Provers cannot **reason inductively**

   - Inductive reasoning deduces a property over integer :ada:`I`

     + If it can be proved for :ada:`I = 0`
     + If it can be proved for :ada:`I+1` from the property for :ada:`I`

   - Solution: lead the prover to this reasoning with a **loop**

--------------------------------------------------
Limitations of Automatic Provers - Proof Context
--------------------------------------------------

* Proof context for a check in a subprogram :ada:`S` is:

  - The contracts of all subprograms called by :ada:`S`
  - The body of :ada:`S` prior to the check
  - The logical modeling of all entities used in :ada:`S`

* Proof context can become **too large**

  - Thousands of lines in the VC
  - This can make the VC unprovable, or hard to prove

* Various solutions to reduce the proof context

  - Split the body of :ada:`S` in smaller subprograms
  - Extract **properties of interest** in lemmas
  - Use special SPARK features

    + Pragma :ada:`Assert_And_Cut`
    + SPARK Library :ada:`SPARK.Cut_Operations`
    + SPARK annotation :ada:`Hide_Info`
      
-----------------------
Cost/Benefit Analysis
-----------------------

* Not all provable properties are worth proving!
* Difficulty of proof (cost) not correlated with benefit
* e.g. proving that a sorting algorithm preserves the components

   - Trivial by review if the only operation is :ada:`Swap`
   - May require many **annotations** for proof

* Functional correctness of complex algorithms is **costly**

   - Specifications can be larger than code
   - Annotations typically much larger than code (:math:`\times` 10)

---------------------------
Dealing with False Alarms
---------------------------

* Check messages can be justified with pragma :ada:`Annotate`

  .. code:: Ada

     pragma Annotate (GNATprove, Category, Pattern, Reason);

  - :ada:`GNATprove` is a fixed identifier
  - :ada:`Category` is one of :ada:`False_Positive` or :ada:`Intentional`

    + :ada:`False_Positive`: check cannot fail
    + :ada:`Intentional`: check can fail but is not a bug

  - :ada:`Pattern` is a substring of the check message

    + Asterisks :ada:`*` match zero or more characters in the message

  - :ada:`Reason` is a string literal for reviews

    + Reason is repeated in output with switch :command:`--report=all` and in
      analysis summary file :filename:`gnatprove.out`

* Justification inserted immediately after the check message location

  - Or at the beginning of a scope

    + Applies to all the scope
    + Generally used when not suitable after the check message location

=====
Lab
=====

.. include:: labs/060_proof.lab.rst

=========
Summary
=========

-------
Proof
-------

* Proof uses Strongest Postcondition Calculus to generate formulas
* Formulas aka Verification Conditions (VC) are sent to provers
* Proof detects:

  - Possible run-time errors
  - Possible failure of assertions
  - Violation of functional contracts (:ada:`Pre` and :ada:`Post`)

* Proof allows to reach Silver/Gold/Platinum levels
* Proof is imprecise

  - On non-linear arithmetic and mixed arithmetic
  - On existential quantification and inductive reasoning
  - When the proof context is too large
