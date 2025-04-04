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

* **Extension** of the CFG with information on **data flows**
* Control Dependence Graph

  - Compute post-dominators nodes: a node z is said to post-dominate a node n
    if **all** paths to the exit node of the graph starting at n must go through z

* Data Dependence Graph

  - Compute def-use chains rooted at variable definitions

* Transitive Dependence Graph

  - Compute how outputs of a call depend on its inputs

* Flow analysis checks are translated into **queries** on the PDG

