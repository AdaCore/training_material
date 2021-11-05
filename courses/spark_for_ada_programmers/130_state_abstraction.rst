
*******************
State Abstraction
*******************
.. |rightarrow| replace:: :math:`\rightarrow`

==============
Introduction
==============

-------------------------
What is an Abstraction?
-------------------------

* Two different views of the same object

   - The abstraction captures what it does
   - The refinement provides more detail on how it is done

* It is a well-supported concept in Ada

   - Packages and subprograms may have both a specification and an implementation
   - The specification and contracts are an abstraction of the body

.. code:: Ada

   procedure Increase (X : in out Integer) with
     Global => null,
     Pre    => X <= 100,
     Post   => X'Old < X;
   procedure Increase (X : in out Integer) is
   begin
     X := X + 1;
   end Increase;

----------------------------
Why is Abstraction Useful?
----------------------------

* The specification summarizes what users may rely on

   - Implementation of one abstraction should not depend on the implementation of another

* Abstraction simplifies implementation and verification

   - The users of an abstraction need only to understand its behavior

* Abstraction simplifies maintenance and code reuse

   - Changes to an object's implementation do not affect its users

.. code:: Ada

   procedure Increase (X : in out Integer) with
     Global => null,
     Pre    => X <= 100,
     Post   => X'Old < X;
   while X <= 100 loop  --  The loop will terminate
     Increase (X);      --  Increase can be called safely
   end loop;
   pragma Assert (X = 101); --  Will this hold ?

===========================
Package State Abstraction
===========================

----------------------------------
Abstraction of a Package's State
----------------------------------

* The variables declared in a package are part of its state
* The state of a package can be

   - Visible...

      + Variables declared in the public part of the package's specification

   - ... Or hidden, allowing abstraction

      + Variables declared in the private part of the package or in its body
      + They are typically accessed through subprogram calls
      + They can be modified without updating other packages

.. code:: Ada

   package Stack is
     procedure Pop  (E : out Element);
     procedure Push (E : in  Element);
   end Stack;
   package body Stack is
     Content : Element_Array (1 .. Max);
     Top     : Natural;

-------------------------------
Declaring a State Abstraction
-------------------------------

* A name, called **State Abstraction**, can be introduced for the hidden state of a package using the `Abstract_State` aspect

   - Several state abstractions can be introduced at once using an aggregate notation
   - It can be introduced equivalently using an `Abstract_State` pragma located at the beginning of the package specification

.. code:: Ada

   package Stack with
     Abstract_State => The_Stack
   is
   package Stack with
     Abstract_State => (Top_State, Content_State)
   is
   package Stack is
     pragma Abstract_State (The_Stack);

----------------------------
Refining an Abstract State
----------------------------

* Each state abstraction must be refined into its constituent using a `Refined_State` aspect

   - This aspect is specified in the package's body
   - It associates each state abstraction to the list of its constituents
   - Every hidden state, such as private variables, must be part of exactly one state abstraction
   - This refinement is mandatory, even if only one state abstraction is declared

.. code:: Ada

   package body Stack with
     Refined_State => (The_Stack => (Content, Top))
   is
     Content : Element_Array (1 .. Max);
     Top     : Natural;
     --  Both Content and Top must be listed in the list of
     --  constituents of The_Stack

--------------------------------
Representing Private Variables
--------------------------------

* The `private` part of a package specification can be visible when its body is not
* In a package with an `Abstract_State`, private state must be associated to state abstractions at declaration

   - It is done using the `Part_Of` aspect on its declaration
   - The hidden state must still be listed in the `Refined_State` aspect

.. code:: Ada

   package Stack with Abstract_State => The_Stack is
     procedure Pop  (E : out Element);
     procedure Push (E : in  Element);
   private
     Content : Element_Array (...)
               with Part_Of => The_Stack;
     Top     : Natural
               with Part_Of => The_Stack;
   end Stack;
   package body Stack with
     Refined_State => (The_Stack => (Content, Top))

===================
Additional States
===================

-----------------
Nested Packages
-----------------

* The state of a package nested inside a package `P` is a part of `P`'s state

   - If the nested package is hidden, its state is part of `P`'s hidden state and must be listed in `P`'s state refinement
   - If the nested package is public, its hidden state must be part of its own (public) state abstraction

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

--------------------------------
Constants with Variable Inputs
--------------------------------

* Constants with variable inputs are considered as variables in contracts

   - A constant has variable inputs if its value depends on a variable, a parameter, or another constant with variable inputs
   - Constants with variable inputs participate to the flow of information between variables
   - Constants with variable inputs are part of the state of a package and must be listed in its state refinement

.. code:: Ada

   package body Stack with
     Refined_State => (The_Stack => (Content, Top, Max))
   is
     Max     : constant Natural := External_Variable;
     Content : Element_Array (1 .. Max);
     Top     : Natural;
     --  Max has variable inputs. It must appear as a
     --  constituent of The_Stack

======================
Subprogram Contracts
======================

--------------------
Global and Depends
--------------------

* State abstractions are used in `Depends` and `Global` contracts in place of the hidden state they represent

   - State abstraction aggregates several variables - results in shorter (more abstract) contracts

.. code:: Ada

   package Stack with
     Abstract_State => (Top_State, Content_State) is
     procedure Pop  (E : out Element) with
       Global  => (Input  => Content_State,
                   In_Out => Top_State),
       Depends => (Top_State => Top_State,
                   E         => (Content, Top_State));
   package Stack with
     Abstract_State => The_Stack is
     procedure Pop  (E : out Element) with
       Global  => (In_Out => The_Stack),
       Depends => ((The_Stack, E) => The_Stack);

--------------------
Global and Depends
--------------------

* `Global` and `Depends` contracts referring to state abstractions can be refined using the `Refined_Global` and `Refined_Depends` aspects

   - The refined aspects are associated with the subprogram's body, where the state refinement is visible
   - They refer directly to hidden state instead of state abstractions
   - Refined `Global` and `Depends` are used for internal calls
   - Refined `Global` and `Depends` contracts are optional (but flow analysis is more precise if they are present)

.. code:: Ada

   package body Stack
     ...
     procedure Pop  (E : out Element) with
       Refined_Global  => (Input  => Content,
                           In_Out => Top),
       Refined_Depends => (Top => Top,
                           E   => (Content, Top)) is

------------------------
Pre and Postconditions
------------------------

* Refinement in pre and postconditions is usually handled using expression functions

   - Inside the package, the body of the expression function can be used for verification
   - Outside the package, the expression function is uninterpreted

.. code:: Ada

   package Stack
     ...
     function Is_Empty return Boolean;
     function Is_Full  return Boolean;
     procedure Push (E : Element) with
       Pre  => not Is_Full,
       Post => not Is_Empty;
   package body Stack
     ...
     function Is_Empty return Boolean is (Top = 0);
     function Is_Full  return Boolean is (Top = Max);

------------------------
Pre and Postconditions
------------------------

* The `Refined_Post` aspect can be used to strengthen a postcondition

   - Like with expression functions, refined postconditions will only be available for internal calls

   - It must be at least as strong as the subprogram's postcondition
   - There are no counterparts for preconditions

.. code:: Ada

   package Stack
     ...
     procedure Push (E : Element) with
       Pre  => not Is_Full,
       Post => not Is_Empty;
   package body Stack
     ...
     procedure Push (E : Element) with
       Refined_Post => not Is_Empty and E = Content (Top);

-----------------------------------
Initialization of Local Variables
-----------------------------------

* The `Initializes` aspect allows specifying the variables initialized during a package's elaboration

   - It is optional; if not provided, an approximation of the set of initialized variables may be computed by the tool
   - If an `Initializes` aspect is provided, it must list all the states (both hidden and visible) initialized during the package's elaboration
   - `Initializes` refers to private and hidden state using state abstractions
   - Note that in SPARK, only local variables can be written at elaboration

.. code:: Ada

   package Stack with
     Abstract_State => The_Stack,
     Initializes    => The_Stack
   is
   -- Flow analysis will make sure both Top and Content are
   -- initialized at package elaboration

-----------------------------------
Initialization of Local Variables
-----------------------------------

* If the initial value of a variable or state abstraction depends on an external variable, the relation must be stated in the `Initializes` aspect

   - Like in `Depends` contracts, relations between variables are represented using an arrow
   - If an initialized state does not depend on any variable defined outside the package, the dependency can be omitted

.. code:: Ada

   package P with
     Initializes => (V1, V2 => External_Variable)
   is
     V1 : Integer := 0;
     V2 : Integer := External_Variable;
   end P;
   --  The association for V1 is omitted, it does not
   -- depend on any external state.

========
Lab
========

.. include:: labs/130_state_abstraction.lab.rst

=========
Summary
=========

---------
Summary
---------

+ Generation of software programs typically deals with *abstraction*

   - Subprograms are an *abstraction* of an operation
   - Objects can be an *abstraction* of physical aspects

+ Proving software programs requires *state abstraction*

   - Allows users to simplify the state of a system/subsytem/package
   - Allows tools to simplify interactions between system/subsystem/package
