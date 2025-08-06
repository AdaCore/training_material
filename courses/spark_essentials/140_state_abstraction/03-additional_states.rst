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
       Content : Component_Array (1 .. Max);
       Top     : Natural;
       --  Max has variable input. It must appear as a
       --  constituent of The_Stack

