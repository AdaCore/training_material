=================
Type Invariants
=================

---------------------------
What Is a Type Invariant?
---------------------------

* Boolean property that should always hold of objects of the type

  - ...**outside** of its unit
  - Same use of name of the type and component names as in predicates

* Can only be specified on the completion of a private type (in SPARK)

  .. code:: Ada

     package Bank is
       type Account is private;
       type Currency is delta 0.01 digits 12;
       ...
     private
       type Account is ... with
         Type_Invariant => Consistent_Balance (Account);

* **Not** part of membership test :ada:`X in T`

-------------------------------------
Dynamic Checking of Type Invariants
-------------------------------------

* Dynamic checks inserted by GNAT

  - When using switch :command:`-gnata`
  - Or pragma :ada:`Assertion_Policy (Type_Invariant => Check)`

* Placement of checks on the creation of values of type :ada:`T`

  - Note: that applies to objects with a part of type :ada:`T`
  - On default initial value
  - On type conversion :ada:`T(...)`
  - On parameter passing after a call to a :dfn:`boundary subprogram`

    + i.e. call to a subprogram in the public spec of the package

* No checks where not needed

  - On assignment and initialization
  - On qualification :ada:`T'(...)`
  - On references to an object
  - On **internal** assignment or call

* No checks where this is impossible for the compiler

  - On **global** variables of type :ada:`T`
  - On parts of objects under components of **access** type

------------------------------------
Static Checking of Type Invariants
------------------------------------

* Static checks performed by :toolname:`GNATprove`

  - **Always** where needed (independent of the choice of switches or pragmas)

* Placement of checks as for dynamic checks

  - **Plus** global variables and objects under access types
  - On each call to external subprogram from inside the unit

    + This avoids so-called :dfn:`reentrancy problems`

  - :toolname:`GNATprove` checks objects **always** satisfy their invariant
    outside of their unit

* No checks only where not needed
* :toolname:`GNATprove` can assume that all inputs to *boundary subprograms* and all objects of the type outside the unit
  satisfy their type invariants

  - Type invariant is used both for proof of unit itself and in other units
  - An expression function deferred to the body can be used to perform an abstraction

-------------------------------------
Beware Recursion in Type Invariants
-------------------------------------

* Infinite recursion when calling inside the type invariant a *boundary
  function* taking the type with invariant as parameter type

  .. code:: Ada

     package Bank is
        type Account is private;
        function Consistent_Balance (A : Account) return Boolean;
     private
        type Account is ... with
          Type_Invariant => Consistent_Balance (Account);

  .. code:: console

     high: cannot call boundary subprogram for type in its own invariant

* Fix by declaring the function in the **private** part of the spec

  .. code:: Ada

     private
        type Account is ... with
          Type_Invariant => Consistent_Balance (Account);
        function Consistent_Balance (A : Account) return Boolean
          is (...);

