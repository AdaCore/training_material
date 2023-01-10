*******************
State Abstraction
*******************

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

---------------------------------------------
Subprogram Contracts and Information Hiding
---------------------------------------------

* Subprogram contracts expose variables and types

  - In preconditions with aspect :ada:`Pre`
  - In postconditions with aspect :ada:`Post`

* Variables and types mentioned directly need to be visible

* Information hiding forbids exposing variables and types

  - Global variables in the private part or body
  - Use of private types for parameters

* Solution is to use (ghost) query functions

  .. code:: ada

       type T is private;
       function Get_Int (X : T) return Integer;
       function Get_Glob return Integer;

       procedure Proc (X : in out T)
       with
         Pre  => Get_Int (X) /= Get_Glob;
         Post => Get_Int (X) = Get_Glob;
     private
       type T is ...   -- returned by Get_Int
       Glob : Integer; -- returned by Get_Glob

---------------------------------------------
Dependency Contracts and Information Hiding
---------------------------------------------

* Dependency contracts expose variables

  - In data dependencies with aspect :ada:`Global`
  - In flow dependencies with aspect :ada:`Depends`

* These variables need to be visible

* Information hiding forbids exposing variables

* Solution is to use :dfn:`state abstraction`

  - Names that denote one or more global variables
  - They represent all the :dfn:`hidden state` of the package

=================
Abstract States
=================

----------------
Abstract State
----------------

* Abstract state declared with aspect :ada:`Abstract_State`

  - On the package spec

  .. code:: Ada

     package Stack with
       Abstract_State => The_Stack
     is ...

* More than one abstract state is possible

  .. code:: Ada

     package Stack with
       Abstract_State => (Top_State, Content_State)
     is ...

* The number of abstract states is a choice

  - More abstract states make the contracts more precise
  - ...but expose more details
  - ...that may not be useful for callers

------------------
State Refinement
------------------

* :dfn:`State refinement` maps each abstract to variables

  - All hidden variables must be constituents of an abstract state
  - This includes variables in the private part and in the body

* Refined state declared with aspect :ada:`Refined_State`

  - On the package body

  .. code:: Ada

     package body Stack with
       Refined_State => (The_Stack => (Top, Content))
     is ...

* More than one abstract state is possible

  .. code:: Ada

     package body Stack with
       Refined_State => (Top_State => Top,
                         Content_State => Content)
     is ...

---------------------------
State in the Private Part
---------------------------

* Private part of package is visible when body is not

  - From client code that only sees the package spec
  - State refinement is not visible in that case
  - What is the abstract state for variables in the private part?

    + This is a problem for flow analysis

* Partial refinement declared with aspect :ada:`Part_Of`

  - On variables in the private part
  - Even when only one abstract state declared

  .. code:: ada

     package Stack with
       Abstract_State => The_Stack
     is ...
     private
        Content : T       with Part_Of => The_Stack;
        Top     : Natural with Part_Of => The_Stack;
     end Stack;

* When package body is present, confirmation in :ada:`Refined_State`

  .. code:: ada

     package body Stack with
       Refined_State => (The_Stack => (Content, Top))

===================
Additional States
===================

-----------------
Nested Packages
-----------------

* State of package :ada:`P` includes state of nested packages :ada:`N`

  - :ada:`N` may have visible state (variables in the public part, abstract states)

  - :ada:`N` may have hidden state (variables in the private part of body)

  - If :ada:`N` is visible

    + Its visible state is visible for :ada:`P` too
    + As are its own abstract states
    + Its hidden state is a constituent of its own abstract states

  - If :ada:`N` is hidden

    + Its visible state is a constituent of :ada:`P`'s abstract states
    + As are its own abstract states
    + Its hidden state is a constituent of its own abstract states

.. code:: Ada

   package P with Abstract_State => State is
     package Visible_Nested with
       Abstract_State => Visible_State is
       ...
   end P;
   package body P with
     Refined_State => (State => Hidden_Nested.Hidden_State)
   is
     package Hidden_Nested with
       Abstract_State => Hidden_State is

----------------
Child Packages
----------------

* State of package :ada:`P` includes state of private child package :ada:`P.Priv`

  - Its visible state is a constituent of :ada:`P`'s abstract states
  - As are its own abstract states
  - Its hidden state is a constituent of its own abstract states

* The visible state of private child packages should have :ada:`Part_Of`

* The state of public child packages is not concerned

.. code:: Ada

   package P with Abstract_State => State is ...

   private package P.Priv with
      Abstract_State => (Visible_State with Part_Of => State)
   is
       Var : T with Part_Of => State;
       ...

   package body P with
     Refined_State => (State => (P.Priv.Visible_State,
                                 P.Priv.Var, ...

-------------------------------
Constants with Variable Input
-------------------------------

* Constants are not part of the package state usually

  - Same for named numbers

  .. code:: ada

     package P is
        C : constant Integer := 42;
        N : constant := 42;

* Some constants are part of the package state

  - When initialized from variables, directly or not
  - They participate in information flow
  - These are :dfn:`constants with variable input`

  .. code:: Ada

     package body Stack with
       Refined_State => (The_Stack => (Content, Top, Max))
     is
       Max     : constant Natural := External_Variable;
       Content : Element_Array (1 .. Max);
       Top     : Natural;
       --  Max has variable input. It must appear as a
       --  constituent of The_Stack

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
      procedure Pop  (E : out Element) with
        Global => (Input  => Content_State,
                   In_Out => Top_State);

   package Stack with
     Abstract_State => The_Stack
   is
      procedure Pop  (E : out Element) with
        Global => (In_Out => The_Stack);

-------------------
Flow Dependencies
-------------------

* Abstract states are used in :ada:`Depends` contracts

.. code:: Ada

   package Stack with
      Abstract_State => (Top_State, Content_State)
   is
      procedure Pop  (E : out Element) with
        Depends => (Top_State => Top_State,
                    E         => (Content_State, Top_State));

   package Stack with
      Abstract_State => The_Stack
   is
       procedure Pop  (E : out Element) with
         Depends => ((The_Stack, E) => The_Stack);

-----------------------
Dependency Refinement
-----------------------

* Inside the body, one can specify refined dependencies

  - Referring to constituents instead of abstract states
  - With aspects for refined dependencies on the subprogram body

    + Aspect :ada:`Refined_Global` for data dependencies
    + Aspect :ada:`Refined_Depends` for flow dependencies

* :toolname:`GNATprove` verifies these specifications when present

* :toolname:`GNATprove` generates those refined contracts otherwise

  - More precise flow analysis inside the unit

========================
Package Initialization
========================

--------------------------------
Data Dependencies of a Package
--------------------------------

* The :dfn:`package elaboration` executes code

  - For all declarations in the package spec
  - For all declarations in the package body
  - And the statements at the end of the package body

* Only package state can be written during package elaboration

  - A package cannot write the state of another package in SPARK

* Aspect :ada:`Initializes` specifies state initialized during elaboration

  - If present, must be complete, including visible and hidden state
  - Otherwise, :toolname:`GNATprove` generates it
  - Similar to the outputs of mode :ada:`Output` for the package elaboration

.. code:: Ada

   package Stack with
      Abstract_State => The_Stack,
      Initializes    => The_Stack
   is
      -- Flow analysis verifies that Top and Content are
      -- initialized at package elaboration.

--------------------------------
Flow Dependencies of a Package
--------------------------------

* Initialization of package state can depend on other packages

  - This dependency needs to be specified in aspect :ada:`Initializes`
  - If no such aspect, :toolname:`GNATprove` also generates these dependencies
  - Similar to the :ada:`Depends` aspect for the package elaboration

.. code:: Ada

   package P with
      Initializes => (V1, V2 => External_Variable)
   is
      V1 : Integer := 0;
      V2 : Integer := External_Variable;
   end P;
   -- The association for V1 is omitted, it does not
   -- depend on any external state.

=====
Lab
=====

.. include:: labs/14_state_abstraction.lab.rst

=========
Summary
=========

-------------------
State Abstraction
-------------------

* Abstract state represents hidden state of a package

  - Variables in the private part or body
  - Visible state of nested packages (variables and abstract states)
  - Visible state of private child packages
  - Constants with variable input

* Each abstract state must be refined into constituents

  - Annotation :ada:`Part_Of` needed on declarations in the private part

* Dependency contracts use abstract states to refer to hidden state

* Initialization at elaboration specified with aspect :ada:`Initializes`

  - This concerns both visible and hidden state
  - This replaces aspects :ada:`Global` and :ada:`Depends` for package
    elaboration
