===========================
Flow Dependency Contracts
===========================

---------------------------------
Basic Data Dependency Contracts
---------------------------------

* Introduced by aspect :ada:`Depends`

* Optional, but must be complete if specified

* Describes how outputs depend on inputs

  .. code:: Ada

     procedure Proc
     with
       Depends => (X => (X, Y),
                   Z => V);

* Not very interesting for functions which have only their result as output

  .. code:: Ada

     function Func (X : Integer)
     with
       Depends => (Func'Result => (X, Y, Z));

-----------------------------------
Some Outputs May Appear As Inputs
-----------------------------------

* Parts of outputs are in fact inputs:

  - Bounds of arrays

  - Discriminants of records

  - Tags of tagged records

* These output objects will appear as inputs in ``Depends`` when
  bounds/discriminants/tags not implied by the object subtype

  .. code:: Ada

     procedure Proc (Tab : out Table)
     with
       Global => (Output => Glob),
       Depends => (Tab  => Tab,
                   Glob => Glob);

---------------
Special Cases
---------------

* Some outputs may depend on no input

  - Typically when initializing data to some constant value
  - Thus, output depends on *null*

  .. code:: Ada

     procedure Init (T : out Table)
     with
       Depends => (T => null);

* Some inputs may not flow into any output

  - Typically when effect hidden from analysis
  - Or input used only for debug
  - Also the case for global variables of mode :ada:`Proof_In`
  - Must be last line of flow dependencies

  .. code:: Ada

     procedure Debug (T : Table)
     with
       Depends => (null => T);

------------------
Special Notation
------------------

* Outputs can also be grouped

  .. code:: Ada

     procedure Init (T1, T2 : out Table)
     with
       Depends => ((T1, T2) => null);

* Symbol **+** indicates a self-dependency

  .. code:: Ada

     procedure Update (T : in out Table)
     with
       Depends => (T => +null);  -- same as (T => T)

* Most useful with grouped outputs

  .. code:: Ada

     procedure Update (T1, T2 : in out Table)
     with
       Depends => ((T1, T2) => +null);
                  -- same as (T1 => T1, T2 => T2)

