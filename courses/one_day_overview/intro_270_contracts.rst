***************
Ada Contracts
***************

..
    Coding language

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

..
    Math symbols

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

--------------------------
:dfn:`Design-By-Contract`
--------------------------

* Source code acting in roles of **client** and **supplier** under a binding **contract**

   - :dfn:`Contract` specifies *requirements* or *guarantees*

      - *"A specification of a software element that affects its use by potential clients."* (Bertrand Meyer)

   - :dfn:`Supplier` provides services

       - Guarantees specific functional behavior
       - Has requirements for guarantees to hold

   - :dfn:`Client` utilizes services

       - Guarantees supplier's conditions are met
       - Requires result to follow the subprogram's guarantees

------------------
:dfn:`Assertion`
------------------

* Boolean expression expected to be :ada:`True`
* Said *to hold* when :ada:`True`
* Language-defined :ada:`pragma`

    - The :ada:`Ada.Assertions.Assert` subprogram can wrap it

.. code:: Ada

   pragma Assert (not Full (Stack));
   -- stack is not full
   pragma Assert (Stack_Length = 0,
                  Message => "stack was not empty");
   -- stack is empty

* Raises language-defined :ada:`Assertion_Error` exception if expression does not hold

.. code:: Ada

   package Ada.Assertions is
     Assertion_Error : exception;
     procedure Assert (Check : in Boolean);
     procedure Assert (Check : in Boolean; Message : in String);
   end Ada.Assertions;

-----------------------
Defensive Programming
-----------------------

* Should be replaced by subprogram contracts when possible

.. code:: Ada

   procedure Push (S : Stack) is
      Entry_Length : constant Positive := Length (S);
   begin
      pragma Assert (not Is_Full (S)); -- entry condition
      [...]
      pragma Assert (Length (S) = Entry_Length + 1); -- exit condition
   end Push;

* Subprogram contracts are an **assertion** mechanism

   - **Not** a drop-in replacement for all defensive code

.. code:: Ada

   procedure Force_Acquire (P : Peripheral) is
   begin
      if not Available (P) then
         -- Corrective action
         Force_Release (P);
         pragma Assert (Available (P));
      end if;

      Acquire (P);
   end;

===================================
Preconditions and Postconditions
===================================

-----------------------------
Subprogram-based Assertions
-----------------------------

* **Explicit** part of a subprogram's **specification**

    - Unlike defensive code

* :dfn:`Precondition`

   - Assertion expected to hold **prior to** subprogram call

* :dfn:`Postcondition`

   - Assertion expected to hold **after** subprogram return

* Requirements and guarantees on both supplier and client
* Syntax uses **aspects**

   .. code:: Ada

      procedure Push (This : in out Stack_T;
                      Value : Content_T)
        with Pre  => not Full (This),
             Post => not Empty (This)
                     and Top (This) = Value;

-------------------------------------
Postcondition :ada:`'Old` Attribute
-------------------------------------

* Values as they were just before the call
* Uses language-defined attribute :ada:`'Old`

   - Can be applied to most any visible object

      + :ada:`limited` types are forbidden
      + May be expensive

   - Expression can be **arbitrary**

        + Typically :ada:`out`, :ada:`in out` parameters and globals

   .. code:: Ada

      procedure Increment (This : in out Integer) with
          Pre  => This < Integer'Last,
          Post => This = This'Old + 1;

-------------------------------------------------
Function Postcondition :ada:`'Result` Attribute
-------------------------------------------------

* :ada:`function` result can be manipulated with :ada:`'Result`

   .. code:: Ada
      function Greatest_Common_Denominator (A, B : Integer)
        return Integer with
          Pre  =>  A > 0 and B > 0,
          Post =>  Is_GCD (A, B,
                           Greatest_Common_Denominator'Result);

================
Type Invariants
================

---------------
Strong Typing
---------------

* Ada supports strong typing

   .. code:: Ada

      type Small_Integer_T is range -1_000 .. 1_000;
      type Enumerated_T is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
      type Array_T is array (1 .. 3) of Boolean;

* What if we need stronger enforcement?

   * Number must be even
   * Subset of non-consecutive enumerals
   * Array should always be sorted
   * Type invariants are only checked on external boundaries

* **Type Invariant**

   * Property of type that is always true on **external** reference
   * *Guarantee* to client, similar to subprogram postcondition

* **Subtype Predicate**

   * Property of type that is always true, unconditionally
   * Can add arbitrary constraints to a type, unlike the "basic" type system

------------------------
Example Type Invariant
------------------------

* A bank account balance must always be consistent

   - Consistent Balance:  Total Deposits  - Total Withdrawals  =  Balance

.. code:: Ada

   package Bank is
     type Account is private with
       Type_Invariant => Consistent_Balance (Account);
     ...
     -- Called automatically for all Account objects
     function Consistent_Balance (This : Account)
       return Boolean;
     ...
   private
     ...
   end Bank;

-----------------------------------
Invariants Don't Apply Internally
-----------------------------------

* No checking within supplier package

   - Otherwise there would be no way to implement anything!

* Only matters when clients can observe state

.. code:: Ada

   procedure Open (This : in out Account;
                   Name : in String;
                   Initial_Deposit : in Currency) is
   begin
     This.Owner := To_Unbounded_String (Name);
     This.Current_Balance := Initial_Deposit;
     -- invariant would be false here!
     This.Withdrawals := Transactions.Empty_List;
     This.Deposits := Transactions.Empty_List;
     This.Deposits.Append (Initial_Deposit);
     -- invariant is now true
   end Open;

====================
Subtype Predicates
====================

----------------
**Predicates**
----------------

* Assertion expected to hold for all objects of given type
* Expressed as any legal boolean expression in Ada

   - Quantified and conditional expressions
   - Boolean function calls

* Two forms in Ada

   - **Static Predicates**

      + Specified via aspect named :ada:`Static_Predicate`

   - **Dynamic Predicates**

      + Specified via aspect named :ada:`Dynamic_Predicate`

----------------------------
Subtype Predicate Examples
----------------------------

* Dynamic Predicate

   .. code:: Ada

      subtype Even is Integer with Dynamic_Predicate =>
         Even mod 2 = 0; -- Boolean expression
         -- (Even indicates "current instance")

* Static Predicate

   .. code:: Ada

      type Serial_Baud_Rate is range 110 .. 115200
        with Static_Predicate => Serial_Baud_Rate  in
          -- Non-contiguous range
          110  | 300  | 600 | 1200 | 2400 | 4800 |
          9600 | 14400 | 19200 | 28800 | 38400 | 56000 |
          57600 | 115200;

--------------------
Predicate Checking
--------------------

* Calls inserted automatically by compiler
* Violations raise exception :ada:`Assertion_Error`

   - When predicate does not hold (evaluates to False)

* Checks are done before value change

   - Same as language-defined constraint checks

* Associated variable is unchanged when violation is detected

------------------------------
Predicate Expression Content
------------------------------

* Reference to value of type itself, i.e., "current instance"

   .. code:: Ada

      subtype Even is Integer
        with Dynamic_Predicate => Even mod 2 = 0;
      J, K : Even := 42;

* Any visible object or function in scope

   - Does not have to be defined before use
   - Relaxation of "declared before referenced" rule of linear elaboration
   - Intended especially for (expression) functions declared in same package spec
