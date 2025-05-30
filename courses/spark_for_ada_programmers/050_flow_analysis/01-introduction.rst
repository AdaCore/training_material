==============
Introduction
==============

------------------------
What Is Flow Analysis?
------------------------

* **First** static analysis performed by :toolname:`GNATprove`
* Models the **variables** used by a subprogram

  - Global variables
  - Scope variables (local variables of enclosing scope)
  - Local variables
  - Formal parameters

* Models how **information flows** through the statements in the subprogram

  - From initial values of variables
  - To final values of variables

* Performs checks and detects **violations**

--------------------------
Control Flow Graph (CFG)
--------------------------

* A representation, using **graph notation**, of all **paths** that might be traversed
  through a program during its execution [Wikipedia]

.. container:: columns

 .. container:: column

    .. code:: ada

       function Is_Positive
         (X : Integer)
         return Boolean
       with Post =>
         Is_Positive'Result = (X > 0)
       is
       begin
          if X > 0 then
             return True;
          else
             return False;
          end if;
       end Is_Positive;

 .. container:: column

    .. image:: control_flow_graph.jpg

--------------------------------
Program Dependence Graph (PDG)
--------------------------------

* Control Dependence Graph (CDG) - control dependencies in a program

  - **Nodes** - statements or blocks
  - **Edges** - represent that execution of one node is dependent on another

* Data Dependence Graph (DDG) - models data flow where edges represent

   - **True dependency** - read after write
   - **Anti-dependency** - write after read
   - **Output dependency** - write after write

* Program Dependence Graph (PDG) - combination of CDG and DDG

   - **Nodes** - statements or operations
   - **Edges** - data dependency edges and control dependency edges

* Transitive Dependence Graph (TDG) - adds transitive edges to PDG

   - If **A** |rightarrow| **B** and **B** |rightarrow| **C** ...
   - ... the TDG adds the edge **A** |rightarrow| **C**

* Flow analysis checks are translated into **queries** on the PDG

