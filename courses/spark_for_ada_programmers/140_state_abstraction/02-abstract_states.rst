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

