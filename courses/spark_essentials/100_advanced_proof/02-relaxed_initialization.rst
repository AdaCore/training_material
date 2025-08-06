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

