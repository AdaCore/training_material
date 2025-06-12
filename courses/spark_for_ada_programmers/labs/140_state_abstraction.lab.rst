=====
Lab
=====

-----------------------
State Abstraction Lab
-----------------------

- Find the :filename:`140_state_abstraction` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- In that directory, open the project :filename:`lab.gpr` in :toolname:`GNAT Studio`

   + Or, on the command-line, do :command:`gnatstudio -P lab.gpr`

- Unfold the source code directory (.) in the project pane

----------------------------
Creating an Abstract State
----------------------------

.. container:: animate 1-

   - Define an abstract state called :ada:`State` to hold all of the state of
     package :ada:`Basics`.

      - The "state" means all global data in the package.
      - Don't forget to add :ada:`Refined_State` to list the content of the state

.. container:: animate 2-

   .. code:: Ada

      package Basics
        with Abstract_State => State
      is

      package body Basics
        with Refined_State => (State => (The_Rec, The_Table))
      is

   - Run :menu:`SPARK` |rightarrow| :menu:`Examine All` to see what happens.

.. container:: animate 3-

   :color-red:`basics.adb:2:36: error: cannot use "The_Rec" in refinement, constituent is not a hidden state of package "Basics"`

   :color-red:`basics.adb:2:45: error: cannot use "The_Table" in refinement, constituent is not a hidden state of package "Basics"`

   - :ada:`Abstract_State` is only for hidden data

      - :ada:`The_Rec` and :ada:`The_Table` are visible to the outside world

   - Move :ada:`The_Rec` and :ada:`The_Table` into the private part of the package

----------------------------
Defining an Abstract State 
----------------------------

.. container:: animate 1-

   - Run :menu:`SPARK` |rightarrow| :menu:`Examine All` to see what happens

.. container:: animate 2-

   :color-red:`basics.ads:69:4: error: indicator Part_Of is required in this context [E0009]`

   :color-red:`basics.ads:69:4: error: "The_Rec" is declared in the private part of package "Basics"`

   :color-red:`basics.ads:70:4: error: indicator Part_Of is required in this context [E0009]`

   :color-red:`basics.ads:70:4: error: "The_Table" is declared in the private part of package "Basics"`

   *(other errors ignored for now)*

   - Global data needs to be part of the state

      - But you cannot refine it in the spec
      - So you need to indicate that :ada:`The_Rec` and :ada:`The_Table` are part of the state

.. container:: animate 3-

   .. code:: Ada

      The_Rec : Rec with Part_Of => State;
      The_Table : Table (1 .. 10) with Part_Of => State;

--------------------------
Using the Abstract State
--------------------------

.. container:: animate 1-

   - Now to address the ignored errors:

      :color-red:`basics.ads:29:28: error: "The_Rec" is undefined (more references follow)`

      :color-red:`basics.ads:34:28: error: "The_Table" is undefined (more references follow)`

   - Update the global contracts to indicate that :ada:`State` is being modified, not
     any particular object.

      - Also need to update dependency contracts, because now data depends on the state,
        not any particular object.

.. container:: animate 2-

   *Some examples*

   .. code:: Ada

      procedure Swap_The_Rec
      with
        Global  => (In_Out => State),
        Depends => (The_Rec => +null);

      procedure Swap_The_Table (I, J : Index)
      with
        Global  => (In_Out => State),
        Depends => (The_Table => +(I, J));

------------------------
Initializing the State
------------------------

.. container:: animate 1-

  - What happens when you perform :menu:`Examine All` now?

.. container:: animate 2-

   :color-red:`basics.ads:2:26: warning: no subprogram exists that can initialize abstract state "Basics.State"`

   - We are not guaranteeing that the global data is initialized.

   - Write subprogram :ada:`Init_The_State` to initialize the global state.

.. container:: animate 3-

   *Package spec*

   .. code:: Ada

      procedure Init_The_State
      with
        Global  => (Output => State),
        Depends => (State => null);

   *Package body*

   .. code:: Ada

      procedure Init_The_State is
      begin
         Init_The_Rec;
         Init_The_Table;
      end Init_The_State;

   - Call the initialization procedure during package elaboration

   - Flow analysis should now show no issues
