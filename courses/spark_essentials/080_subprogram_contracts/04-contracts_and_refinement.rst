==========================
Contracts and Refinement
==========================

--------------------
What's Refinement?
--------------------

* :dfn:`Refinement` = relation between two representations

  - An :dfn:`abstract` representation
  - A :dfn:`concrete` representation

* Concrete behaviors are **included** in abstract behaviors

  - Analysis on the abstract representation
  - Findings are valid on the concrete one

* SPARK uses refinement

  - For analysis of **callbacks**
  - For analysis of **dispatching calls** in OOP

    + aka Liskov Substitution Principle (LSP)

* Generics do not follow refinement in SPARK

  - Reminder: instantiations are analyzed instead

------------------------
Contracts on Callbacks
------------------------

* Contracts can be defined on access-to-subprogram types

  - Only precondition and postcondition

  .. code:: ada

     type Update_Proc is access procedure (X : in out Natural)
     with
       Pre  => Precond (X),
       Post => Postcond (X'Old, X);

* :toolname:`GNATprove` checks refinement on **actual** subprograms

  .. code:: ada

     Callback : Update_Proc := Proc'Access;

  - **Precondition** of :ada:`Proc` should be **weaker** than :ada:`Precond(X)`
  - **Postcondition** of :ada:`Proc` should be **stronger** than
    :ada:`Postcond(X'Old, X)`
  - Data **dependencies** should be :ada:`null`

     + **No** use of globals

* :toolname:`GNATprove` uses contract of :ada:`Update_Proc` when
  :ada:`Callback` is called

-------------------
Contracts for OOP
-------------------

* Inherited contracts can be defined on dispatching subprograms

  .. code:: ada

     type Object is tagged record ...
     procedure Proc (X : in out Object) with
       Pre'Class  => Precond (X),
       Post'Class => Postcond (X'Old, X);

* :toolname:`GNATprove` checks refinement on **overriding** subprograms

  .. code:: ada

     type Derived is new Object with record ...
     procedure Proc (X : in out Derived) with ...

  - **Precondition** of :ada:`Proc` should be **weaker** than :ada:`Precond(X)`
  - **Postcondition** of :ada:`Proc` should be **stronger** than
    :ada:`Postcond(X'Old, X)`
  - Data **dependencies** should be the **same**

* :toolname:`GNATprove` uses contract of :ada:`Proc` in :ada:`Object` when
  :ada:`Proc` is called with static type :ada:`Object`

  - Dynamic type might be :ada:`Derived`

