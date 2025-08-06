=============================
Access-to-subprogram Values
=============================

-----------------------------------------
Contracts on Access-to-subprogram Types
-----------------------------------------

* Access-to-subprogram values not subject to ownership

* Only preconditions and postconditions are allowed

  .. code:: ada

     type Proc is access procedure (...)
     with
       Pre  => ...
       Post => ...

* Very often using :ada:`not null access` (for parameters)

* Implicit `Global => null` on type

* :toolname:`GNATprove` checks feasibility of contract 

* Creating a value of access-to-subprogram type with attribute :ada:`Access`

  .. code:: ada

     procedure P (...);
     Acc : Proc := P'Access;

* :toolname:`GNATprove` checks conditions for refinement

  - Pre of type implies pre of subprogram

  - Post of subprogram implies post of type

-----------------------------
Higher Order Specialization
-----------------------------

* Higher order functions take an anonymous access-to-subprogram parameter

* Example of map:

  .. code:: ada

     function Map
       (A : Nat_Array;
        F : not null access function (N : Natural) return Natural)
        return Nat_Array;

* Function ``F`` above cannot read global variables

* Annotation ``Higher_Order_Specialization`` allowed on `Map`

  - Call to ``Map (A, Func'Access)`` specialized for ``Func``

  - ``Func`` is allowed to read global variables

  - ``Func`` can have a precondition and postcondition

* Used in SPARK Higher Order Library

  - Associated lemmmas also use annotation ``Higher_Order_Specialization``

  - Lemmas specialized when calls are specialized

--------------------
Interrupt Handlers
--------------------

* Handler can be called asynchronously outside SPARK program

  - But not called from SPARK code

* Handler declared with access-to-subprogram type

* Handler may read or write global data

* Annotation ``Handler`` on access-to-subprogram type

  .. code:: ada

     type No_Param_Proc is access procedure with
       Annotate => (GNATprove, Handler);

* Can take ``Access`` on subprogram that reads or writes global

  .. code:: ada

     procedure Reset with Global => ...
     P : No_Param_Proc := Reset'Access;

