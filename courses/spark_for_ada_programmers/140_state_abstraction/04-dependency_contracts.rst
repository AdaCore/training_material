======================
Dependency Contracts
======================

-------------------
Data Dependencies
-------------------

* Abstract states are used in :ada:`Global` contracts

  - Abstract state represents all its constituents
  - Mode is the aggregate of all modes of constituents

    + As if the abstract state was a record with constituents as components

.. code:: Ada

   package Stack with
      Abstract_State => (Top_State, Content_State)
   is
      procedure Pop  (E : out Component) with
        Global => (Input  => Content_State,
                   In_Out => Top_State);

   package Stack with
     Abstract_State => The_Stack
   is
      procedure Pop  (E : out Component) with
        Global => (In_Out => The_Stack);

-------------------
Flow Dependencies
-------------------

* Abstract states are used in :ada:`Depends` contracts

.. code:: Ada

   package Stack with
      Abstract_State => (Top_State, Content_State)
   is
      procedure Pop  (E : out Component) with
        Depends => (Top_State => Top_State,
                    E         => (Content_State, Top_State));

   package Stack with
      Abstract_State => The_Stack
   is
       procedure Pop  (E : out Component) with
         Depends => ((The_Stack, E) => The_Stack);

-----------------------
Dependency Refinement
-----------------------

* Inside the body, one can specify refined dependencies

  - Referring to constituents instead of abstract states
  - With aspects for refined dependencies on the subprogram body

    + Aspect :ada:`Refined_Global` for data dependencies
    + Aspect :ada:`Refined_Depends` for flow dependencies

|

* :toolname:`GNATprove` verifies these specifications when present

|

* :toolname:`GNATprove` generates those refined contracts otherwise

  - More precise flow analysis inside the unit

