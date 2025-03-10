****************
Advanced Proof
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

--------------
Proof So Far
--------------

* Variables follow data initialization policy

  - Flow analysis deals with initialization
  - Arrays must be initialized by aggregates
  - Variables cannot be partially/conditionally initialized

* Loop-free code

  - Strongest Postcondition calculus does not deal with loops

    + At least, not without a little help

* How do we deal with the following program?

  .. code:: ada

     procedure Init_Table (T : out Table) is
     begin
        for J in T'Range loop
           T(J) := 0;
        end loop;
     end Init_Table;

.. container:: speakernote

   :ada:`T` is progressively initialized in a loop.
   The techniques we saw so far can't deal with this program.

--------------------------
Going Beyond Basic Proof
--------------------------

* Relaxed initialization

  - Ability to partially initialize variables
  - Proof deals with initialization of such variables

|

* Loop pragmas

  - Specialized pragmas to deal with loops in proof
  - Loop invariants provide the necessary help
  - Loop variants deal with loop termination

|

* SPARK formal containers

  - Dealing with loops over vectors, lists, sets and maps

========================
Relaxed Initialization
========================

------------------------------------------
Limitations of the Initialization Policy
------------------------------------------

* Objects must be fully initialized when read

  - Forces useless initialization of unread components

|

* Arrays must be initialized from an aggregate

  - Otherwise flow analysis cannot check initialization

  - Except in some special cases when a heuristic works

    + e.g. fully initialize an array with a *for loop*

|

* All outputs must be fully initialized when returning

  - Forces useless initialization of unread outputs

-----------------------------------
Specifying Relaxed Initialization
-----------------------------------

* Aspect :ada:`Relaxed_Initialization` can be used on objects, types and subprograms

  .. code:: Ada

     type Rec is record ... end record
       with Relaxed_Initialization;
     X : Integer with Relaxed_Initialization;
     procedure Update (A : in out Arr)
       with Relaxed_Initialization => A;

* Corresponding objects (variables, components) have relaxed initialization

  - Flow analysis does not check (full) initialization
  - Instead, proof checks (partial) initialization when read
  - Not applicable to scalar parameter or scalar function result

------------------------------
Specifying Initialized Parts
------------------------------

* Ghost attribute :ada:`Initialized` is used to specify initialized objects

  .. code:: Ada

     pragma Assert (R'Initialized);

* Or initialization of parts of objects

  .. code:: Ada

     pragma Assert (R.C'Initialized);

* Attribute executed like :ada:`Valid_Scalars`

  - All scalar subcomponents are dynamically checked to be valid values of
    their type

---------------------------------------
Relaxed Initialization and Predicates
---------------------------------------

* Ghost attribute :ada:`Initialized` cannot be used in predicate

  - Rationale: predicate is part of membership tests

* Use instead special :ada:`Ghost_Predicate`

  - Membership tests are not allowed for such types

  - Otherwise subject to same rules as other predicates

  .. code:: Ada

     type Stack is record
        Top     : Index;
        Content : Content_Table;
     end record
        with Ghost_Predicate =>
	   Content (1 .. Top)'Initialized;

----------------------------------
Verifying Relaxed Initialization
----------------------------------

* Contracts (postcondition, predicate) may refer to :ada:`Initialized`

  .. code:: Ada

     procedure Update (R : in out Rec) with
       Post => R'Initialized;

* Any read of an object requires its initialization

* Loop invariant may need to state what part of an array is initialized

  .. code:: Ada

     for J in Arr'Range loop
       Arr(J) := ...
       pragma Loop_Invariant
         (Arr(Arr'First .. J)'Initialized;
     end loop;

=======
Loops
=======

-----------------
Unrolling Loops
-----------------

* :toolname:`GNATprove` can unroll loops when:

  - Loop is of the form :ada:`for J in A .. B loop`
  - Number of iterations is less than 20
  - The only local variables declared in the loop are scalars

* Confirming message issued when using switch :command:`--info`

  .. code:: console

     info: unrolling loop

* Strongest Postcondition calculus can deal with unrolled loop

  - But size of code might become large
  - Especially on nested loops

* Loop unrolling can be prevented

  - Globally with switch :command:`--no-loop-unrolling`
  - On a specific loop with a loop invariant

-----------------
Loop Invariants
-----------------

* A :dfn:`loop invariant` is a special assertion

  - Placed inside loops
  - Executed like an assertion at runtime
  - Interpreted specially in proof
  - Slightly different from classical Hoare loop invariant

* Dynamic checks inserted by GNAT

  - When using switch :command:`-gnata`
  - Or pragma :ada:`Assertion_Policy (Loop_Invariant => Check)`

* Multiple loop invariants are allowed

  - Must be grouped
  - Same as conjunction of conditions using :ada:`and`

* Placement anywhere in the top-level sequence of statements

  - Typically at the beginning or end of the loop
  - Can be inside the statements of a *declare block*
  - Default loop invariant of :ada:`True` at beginning of the loop

--------------------------
Loop Invariants in Proof
--------------------------

* The loop invariant acts as a cut point for the SP calculus

  - Establish it at the beginning of the loop
  - Check that it is preserved by one iteration
  - Assume it to check the remaining of the program

.. image:: loop_invariants.png

------------------------------
Placement of Loop Invariants
------------------------------

.. container:: columns

 .. container:: column

    * Proof reasons around the *virtual loop*

      - Starting from the loop invariant
      - Ending at the loop invariant

 .. container:: column

    .. image:: loop_invariants_placement.png

------------------------------------------
Four Properties of a Good Loop Invariant
------------------------------------------

* These four properties should be established in this order

* [INIT] - It should hold in the first iteration of the loop

   - :toolname:`GNATprove` generates a loop invariant initialization check

* [INSIDE] - It should allow proving absence of run-time errors and local
  assertions inside the loop

* [AFTER] - It should allow proving absence of run-time errors, local
  assertions and the subprogram postcondition after the loop

* [PRESERVE] - It should be preserved by the loop

   - :toolname:`GNATprove` generates a loop invariant preservation check

-----------------------
Summarizing Mutations
-----------------------

* Analysis of arbitrary loop iteration in coarse context

  - All information on modified variables is lost
  - Except information preserved in the loop invariant

* Example: initialization loop

  .. code:: ada

     procedure Init_Table (T : out Table)
     with
       Post => (for all J in T'Range => T(J) = 0);

     procedure Init_Table (T : out Table) is
     begin
        for J in T'Range loop
           T(J) := 0;
           pragma Loop_Invariant
             (for all K in T'First .. J => T(K) = 0);
        end loop;
     end Init_Table;

--------------------------
Accumulating Information
--------------------------

* Analysis of arbitrary loop iteration in coarse context

  - All information accumulated on variables is lost
  - Except information preserved in the loop invariant

* Example: search loop

  .. code:: ada

     procedure Search_Table (T : Table; Found : out Boolean)
     with
       Post => Found = (for some J in T'Range => T(J) = 0);

     procedure Search_Table (T : Table; Found : out Boolean) is
     begin
        for J in T'Range loop
           if T(J) = 0 then
              return True;
           end if;
           pragma Loop_Invariant
             (for all K in T'First .. J => T(K) /= 0);
        end loop;
        return False;
     end Search_Table;

------------------------------
Attribute :ada:`Loop_Entry`
------------------------------

* Attribute :ada:`Loop_Entry` used to refer to the value of a variable on
  entry to the loop

  .. code:: ada

     procedure Bump_Table (T : in out Table) is
     begin
        for J in T'Range loop
           T(J) := T(J) + 1;
           pragma Loop_Invariant
             (for all K in T'First .. J => T(K) = T'Loop_Entry(K) + 1);
        end loop;
     end Bump_Table;

* Similar to attribute :ada:`Old` which is usable only inside postconditions

  - In many cases, :ada:`X'Loop_Entry` is also value on subprogram entry
  - Same limitations as for attribute :ada:`Old`

    + Use :ada:`pragma Unevaluated_Use_Of_Old (Allow)` if needed

* Use :ada:`X'Loop_Entry(Loop_Name)` for value of :ada:`X` on entry to loop
  not directly enclosing

----------------------------
Loop Frame Condition (1/2)
----------------------------

* Reminder: analysis of arbitrary loop iteration in coarse context

  - All information on modified variables is lost
  - Except information preserved in the loop invariant

|

* This is true for the :dfn:`loop frame condition`

  - Variables that are not modified
  - Parts of modified variables that are preserved
  - Similar to frame condition on subprogram calls

|

* :toolname:`GNATprove` generates part of the frame condition

  - Variables that are not modified, or only on paths that exit the loop
  - Components of records that are not modified
  - Components of arrays that are not modified

    + When the array is only assigned at the current loop index

----------------------------
Loop Frame Condition (2/2)
----------------------------

* In other cases, explicit frame condition might be needed

* Typically use attribute :ada:`Loop_Entry`

  .. code:: ada

     procedure Bump_Table (T : in out Table) is
     begin
        for J in T'Range loop
           T(J) := T(J) + 1;
           pragma Loop_Invariant
             (for all K in J .. T'Last =>
                (if K > J then T(K) = T'Loop_Entry(K)));
        end loop;
     end Bump_Table;

.. container:: speakernote

   We don't use "(for all K in J+1 .. T'Last =>" here, as that could
   lead to an index overflow.
   Hence the use of an if-expression.

---------------------------
Classical Loop Invariants
---------------------------

* Known best loop invariants for some loops

  - Initialization loops - initialize the collection
  - Mapping loops - map each component of the collection
  - Validation loops - check each component of the collection
  - Counting loops - count components with a property
  - Search loops - search component with a property
  - Maximize loops - search component that maximizes a property
  - Update loops - update each component of the collection

|

* SPARK User's Guide gives detailed loop invariants

  - See section *7.9.2 Loop Examples*
  - Loops on arrays or formal containers

-----------------------------
Quiz: Non-terminating Loops
-----------------------------

What's wrong with the following code?

.. code:: ada

   loop
      null;
   end loop;
   pragma Assert (False);

.. container:: animate

   * Loop does not terminate

   * :toolname:`GNATprove` proves the assertion of :ada:`False`!

     - Because that program point is unreachable (dead code)

   * :toolname:`GNATprove` implements defense in depth

     - Non-terminating loop causes enclosing subprogram to also not terminate
     - Switch :command:`--proof-warnings=on` can detect dead code
     - Proof of loop termination based on loop variants

---------------------
Loop Variants (1/2)
---------------------

* A :dfn:`loop variant` is a special assertion

  - Placed inside loops
  - Executed specially at runtime
  - Interpreted specially in proof

|

* Dynamic checks inserted by GNAT

  - When using switch :command:`-gnata`
  - Or pragma :ada:`Assertion_Policy (Loop_Variant => Check)`
  - Check that expression varies as indicated at each iteration

|

* Only one loop variant is needed to prove loop termination

  - And only on *while loop* or *plain loop*, not on *for loop*

|

* Same placement as for loop invariants

  - Must be grouped if both presents

---------------------
Loop Variants (2/2)
---------------------

* Same syntax as subprogram variants

  .. code:: ada

     procedure Bump_Table (T : in out Table) is
        J : Index'Base := T'First;
     begin
        while J <= T'Last loop
           T(J) := T(J) + 1;
           J := J + 1;
           pragma Loop_Variant (Increases => J);
        end loop;
     end Bump_Table;

* Could also use :ada:`(Decreases => -J)`

* Same loop variant could be placed anywhere in the loop here

  - Because check between two successive evaluations of the variant
  - The loop invariant must be modified to reflect current values

===================
Formal Containers
===================

-------------------------------
Formal Containers in SPARKlib
-------------------------------

* Available from SPARK Library

  - Distributed with SPARK Pro
  - Copy :filename:`sparklib.gpr` or :filename:`sparklib_light.gpr` locally
  - Set value of :code:`Object_Dir` in the copied project file
  - To use, add :code:`with "sparklib[_light]";` in your project file

* Reminder: four kinds of formal containers

  - vectors
  - doubly linked lists
  - sets (hashed and ordered)
  - maps (hashed and ordered)

* All available in bounded and unbounded versions

* All generics that need to be instantiated

  - Only their spec is in SPARK
  - Their implementation is not proved

---------------------------
Bounded Formal Containers
---------------------------

* Bounded version for light and embedded runtimes

* Under :ada:`SPARK.Containers.Formal.<name>`

* Use discriminated record

  - Discriminant :ada:`Capacity` fixes maximum size

* Component type must have known size (:dfn:`definite` type)

* Container type itself is definite

  - Bounded container can be component of another formal container

-----------------------------
Unbounded Formal Containers
-----------------------------

* Unbounded version for full runtimes

* Under :ada:`SPARK.Containers.Formal.Unbounded_<name>`

* Use dynamic memory allocation

  - For each component in the container
  - For growing the container

* Use controlled types for dynamic memory reclamation

* Component type may have unknown size (:dfn:`indefinite` type)

* Container type itself is definite

  - Unbounded container can be component of another formal container

------------------------------
Loops Over Formal Containers
------------------------------

* Same as for quantified expressions

* Range-based iteration (only for vectors)

  .. code:: ada

     for J in V.First_Index .. V.Last_Index loop
        V.Replace_Element (J, 0);
     end loop;

* Iteration over positions

  .. code:: ada

     for J in V loop
        V.Replace_Element (J, 0);
     end loop;

* Iteration over components (no update!)

  .. code:: ada

     for E of V loop
        pragma Assert (E = 0);
     end loop;

----------------------------------------
Loop Invariants Over Formal Containers
----------------------------------------

* Range-based iteration (only for vectors)

  - Use scalar index :ada:`J` to access vector at :ada:`V.Element (J)`

|

* Iteration over positions

  - For vectors, same as range-based iteration (cursor is index)
  - Otherwise, need to reason about formal model

    + Functional model of the container
    + Mapping from cursors to positions
    + Sequence of components/keys of the container

|

* Iteration over components

  - Impossible to access previous components
  - Use iteration over positions instead

-----------------------------------
Formal Model of Formal Containers
-----------------------------------

* Defined in local package :ada:`Formal_Model`

  - Based on functional containers (also part of SPARKlib)

    + Immutable containers to represent mathematical one

  - Used in contracts of formal containers API

* Functional model of the container

  - Given by function :ada:`Model`
  - Returns a different type

    + A sequence of components for formal lists
    + A set of components for formal sets
    + A map from keys to components for maps

* Mapping from cursors to positions

  - Given by function :ada:`Positions`
  - Positions in the iteration sequence

* Sequence of components/keys of the container

  - Corresponds to the iteration sequence
  - Given by different functions

    + :ada:`Model` for lists
    + :ada:`Elements` for sets
    + :ada:`Keys` for maps

------------------------------------------------
Difficulties with Loops Over Formal Containers
------------------------------------------------

* :toolname:`GNATprove` does not unroll such loops

|

* :toolname:`GNATprove` does not generate a frame condition

  - Contrary to loops over arrays
  - Need to explicitly state the frame condition using attribute
    :ada:`Loop_Entry`

|

* Container structure may be modified in the loop

  - When inserting or deleting components
  - In general, need to know position of corresponding cursor

    + Relative to current cursor: e.g. previous/next cursor
    + Otherwise difficult with hashed sets/maps

----------------------
Functional Containers
----------------------

* Available from SPARK Library

* Five kinds of functional containers

  - infinite sequences
  - vectors
  - sets
  - multisets
  - maps

* Simple containers close to mathematical structures

  - No bounds on cardinality
  - No cursors for iteration
  - No order of components in sets and maps
  - Functional: cannot modify them, rather create a new one

* They are easy to handle for proof

  - Often used as models for more complex structures

* They are executable but might be inefficient

=====
Lab
=====

.. include:: labs/10_advanced_proof.lab.rst

=========
Summary
=========

----------------
Advanced Proof
----------------

* Use relaxed initialization when needed

  - Some variables are partially initialized
  - Some array variables are initialized in a loop
  - More annotations are needed with ghost attribute :ada:`Initialized`

* Proof of loops requires more work

  - Add loop invariants to prove correction
  - Take special care of the loop frame condition
  - Add loop variants to prove termination

* Formal containers

  - Generics for vectors, lists, sets and maps
  - Available in all runtime libraries
  - Proof of code using formal containers uses formal models
