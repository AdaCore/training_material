**********************
Subprogram Contracts
**********************

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

---------------
Ada Contracts
---------------

* Ada contracts include enforcement

   - At compile-time: specific constructs, features, and rules
   - At run-time: language-defined and user-defined exceptions

* Existing facilities

   - Range specifications
   - Parameter modes
   - Generic contracts
   - OOP :ada:`interface` types (Ada 2005)
   - Work well, but on a restricted set of use-cases

* Contracts aspects exist in Ada 95

   - As a set of GNAT-specific :ada:`pragma`
   - Carried by subprograms

------------------
:dfn:`Assertion`
------------------

* Boolean expression expected to be :ada:`True`
* Said *to hold* when :ada:`True`
* Language-defined :ada:`pragma`

.. code:: Ada

   pragma Assert (not Full (Stack));
   -- stack is not full
   pragma Assert (Stack_Length = 0,
                  Message => "stack was not empty");
   -- stack is empty

* Raises language-defined :ada:`Assertion_Error` exception if expression does not hold
* The :ada:`Ada.Assertions.Assert` subprogram wraps it

.. code:: Ada

   package Ada.Assertions is
     Assertion_Error : exception;
     procedure Assert (Check : in Boolean);
     procedure Assert (Check : in Boolean; Message : in String);
   end Ada.Assertions;

------
Quiz
------

Which of the following statements is (are) correct?

    A. :answer:`Assertions can be used in declarations`
    B. Assertions can be used in expressions
    C. :answer:`Any corrective action should happen before contract checks`
    D. Assertions must be checked using :ada:`pragma Assert`

.. container:: animate

    Explanations

    A. Will be checked at elaboration
    B. No assertion expression, but :ada:`raise` expression exists
    C. Exceptions as flow-control adds complexity, prefer a proactive :ada:`if` to a (reactive) :ada:`exception` handler
    D. You can call :ada:`Ada.Assertions.Assert`, or even directly :ada:`raise Assertion_Error`

------
Quiz
------

Which of the following statements is (are) correct?

    A. :answer:`Defensive coding is a good practice`
    B. Contracts can replace all defensive code
    C. Contracts are executable constructs
    D. Having exhaustive contracts will prevent runtime errors

.. container:: animate

    Explanations

    A. Principles are sane, contracts extend those
    B. See previous slide example
    C. e.g. generic contracts are resolved at compile-time
    D. A failing contract **will cause** a runtime error, only extensive (dynamic / static) analysis of contracted code may provide confidence in the absence of runtime errors (AoRTE)

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
* Syntax uses **aspects**, set by pragma in Ada95

   .. code:: Ada

      procedure Push (This : in out Stack_T;
                      Value : Content_T);
      pragma Pre (not Full (This));
      pragma Post (not Empty (This)
                   and Top (This) = Value);

---------------------------------
Requirements / Guarantees: Quiz
---------------------------------

* Given the following piece of code

    .. code:: Ada

       procedure Start is
       begin
           ...
           Turn_On;
           ...

       procedure Turn_On;
       pragma Pre (Has_Power);
       pragma Post (Is_On);

* Complete the table in terms of requirements and guarantees

.. list-table::

   * -
     - Client (:ada:`Start`)
     - Supplier (:ada:`Turn_On`)

   * - Pre (:ada:`Has_Power`)
     - :animate:`Requirement`
     - :animate:`Guarantee`

   * - Post (:ada:`Is_On`)
     - :animate:`Guarantee`
     - :animate:`Requirement`

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

-----------------------------
Pre/Postcondition Semantics
-----------------------------

* Calls inserted automatically by compiler

|

.. image:: pre_and_post_insertion_flow.svg
   :width: 90%

-------------------------------------
Contract with Quantified Expression
-------------------------------------

* Pre- and post-conditions can be **arbitrary** :ada:`Boolean` expressions

.. code:: Ada

   type Status_Flag is (Power, Locked, Running);

   procedure Clear_All_Status (
       Unit : in out Controller);
     -- guarantees no flags remain set after call
   pragma Post ((for all Flag in Status_Flag =>
                 not Status_Indicated (Unit, Flag)));

   function Status_Indicated (
       Unit : Controller;
       Flag : Status_Flag)
       return Boolean;

-------------------------------------
Visibility for Subprogram Contracts
-------------------------------------

* **Any** visible name

   - All of the subprogram's **parameters**
   - Can refer to functions **not yet specified**

      - Must be declared in same scope
      - Different elaboration rules for expression functions

  .. code:: Ada

     function Top (This : Stack) return Content;
     pragma Pre (not Empty (This));
     function Empty (This : Stack) return Boolean;

* :ada:`Post` has access to special attributes

    - See later

------------------------------------------
Preconditions and Postconditions Example
------------------------------------------

* Multiple pragma separated by commas

.. code:: Ada

     procedure Push (This : in out Stack;
                     Value : Content);
     pragma Pre (not Full (This));
     pragma Post (not Empty (This) and Top (This) = Value);

------------------------------------
(Sub)Types Allow Simpler Contracts
------------------------------------

* Pre-condition

   .. code:: Ada

      procedure Compute_Square_Root (Input : Integer;
                                     Result : out Natural);
      pragma Pre (Input >= 0);
      pragma Post ((Result * Result) <= Input and
                   (Result + 1) * (Result + 1) > Input);

* Subtype

   .. code:: Ada

      procedure Compute_Square_Root (Input  : Natural;
                                     Result : out Natural);
      -- "pragma Pre (Input >= 0)" not needed
      -- (Input can't be < 0)
      pragma Post
         ((Result * Result) <= Input and
          (Result + 1) * (Result + 1) > Input);

------
Quiz
------

.. code:: Ada

   function Area (L : Positive; H : Positive) return Positive is
      (L * H);
   pragma Pre (???);

Which pre-condition is necessary for :ada:`Area` to calculate the correct result for
all values :ada:`L` and :ada:`H`

   A. ``L > 0 and H > 0``
   B. ``L < Positive'Last and H < Positive'Last``
   C. ``L * H in Positive``
   D. :answer:`None of the above`

.. container:: animate

   Explanations

   A. Parameters are :ada:`Positive`, so this is unnecessary
   B. :ada:`L = Positive'Last and H = Positive'Last` will cause an overflow
   C. Classic trap: the check itself may cause an overflow!

   Preventing an overflow requires using the expression :ada:`Integer'Last / L <= H`

====================
Special Attributes
====================

--------------------------------------------
Evaluate an Expression on Subprogram Entry
--------------------------------------------

* Post-conditions may require knowledge of a subprogram's **entry context**

  .. code:: Ada

      procedure Increment (This : in out Integer);
      pragma Post (???); -- how to assert incrementation of `This`?

* Language-defined attribute :ada:`'Old`
* Expression is **evaluated** at subprogram entry

   - After pre-conditions check
   - Makes a copy

        + :ada:`limited` types are forbidden
        + May be expensive

   - Expression can be **arbitrary**

        + Typically :ada:`in out` parameters and globals

   .. code:: Ada

      procedure Increment (This : in out Integer);
      pragma Pre (This < Integer'Last);
      pragma Post (This = This'Old + 1);

-----------------------------------
Example for Attribute :ada:`'Old`
-----------------------------------

   .. code:: Ada

      Global : String := Init_Global;
      ...
      procedure Shift_And_Advance (Index : in out Integer) is
      begin
         Global (Index) := Global (Index + 1);
         Index          := Index + 1;
      end Shift_And_Advance;

* Note the different uses of `'Old` in the postcondition

   .. code:: Ada

      procedure Shift_And_Advance (Index : in out Integer);
      pragma Post
         (-- call At_Index before call
         Global (Index)'Old
            -- look at Index position in Global before call
            = Global'Old (Index'Old)
         and
         -- call At_Index after call with original Index
         Global (Index'Old)
            -- look at Index position in Global after call
            = Global (Index));

------------------------------------------------
Error on Conditional Evaluation of :ada:`'Old`
------------------------------------------------

* This code is **incorrect**

.. code:: Ada

  procedure Clear_Character (In_String : in out String;
                             At_Position : Positive);
  pragma Post
    ((if Found_At in In_String'Range
     then In_String (Found_At)'Old = ' '));

* Copies :ada:`In_String (Found_At)` on entry

   - Will raise an exception on entry if :ada:`Found_At not in In_String'Range`
   - The postcondition's :ada:`if` check is not sufficient

* Solution requires a full copy of :ada:`In_String`

.. code:: Ada

  procedure Clear_Character (In_String : in out String;
                             At_Position : Positive);
  pragma Post
    ((if Found_At in In_String'Range
     then In_String'Old (Found_At) = ' '));

-------------------------------------------
Postcondition Usage of Function Results
-------------------------------------------

* :ada:`function` result can be read with :ada:`'Result`

.. code:: Ada

  function Greatest_Common_Denominator (A, B : Positive)
    return Positive;
  pragma Post
    (Is_GCD (A, B, Greatest_Common_Denominator'Result));

------
Quiz
------

.. code:: Ada

   type Index_T is range 1 .. 100;
   -- Database initialized such that value for element at I = I
   Database : array (Index_T) of Integer;
   -- Set the value for element Index to Value and
   -- then increment Index by 1
   function Set_And_Move (Value :        Integer;
                          Index : in out Index_T)
                          return Boolean;
   pragma Post (...);

Given the following expressions, what is their value if they are evaluated in the postcondition
of the call :ada:`Set_And_Move (-1, 10)`

.. list-table::

   * - ``Database'Old (Index)``

     - :animate:`11`
     - :animate:`Use new index in copy of original Database`

   * - ``Database (Index`Old)``

     - :animate:`-1`
     - :animate:`Use copy of original index in current Database`

   * - ``Database (Index)'Old``

     - :animate:`10`
     - :animate:`Evaluation of Database (Index) before call`

----------
Examples
----------


.. container:: columns

  .. container:: column

    .. container:: latex_environment tiny

      .. include:: examples/adv_270_subprogram_contracts/special_attributes_spec.rst

  .. container:: column

    .. container:: latex_environment tiny

      .. include:: examples/adv_270_subprogram_contracts/special_attributes_body.rst

=============
In Practice
=============

----------------------------------------
Pre/Postconditions: to Be or Not to Be
----------------------------------------

* **Preconditions** are reasonable **default** for runtime checks
* **Postconditions** advantages can be **comparatively** low

   - Use of :ada:`'Old` and :ada:`'Result` with (maybe deep) copy
   - Very useful in **static analysis** contexts (Hoare triplets)

* For **trusted** library, enabling **preconditions only** makes sense

   - Catch **user's errors**
   - Library is trusted, so :ada:`Post => True` is a reasonable expectation

* Typically contracts are used for **validation**
* Enabling subprogram contracts in production may be a valid trade-off depending on...

   - Exception failure **trace availability** in production
   - Overall **timing constraints** of the final application
   - Consequences of violations **propagation**
   - Time and space **cost** of the contracts

* Typically production settings favour telemetry and off-line analysis

-------------------------------------
No Secret Precondition Requirements
-------------------------------------

* Client should be able to **guarantee** them
* Enforced by the compiler

.. code:: Ada

   package P is
     function Foo return Bar;
     pragma Pre (Hidden); -- illegal private reference
   private
     function Hidden return Boolean;
   end P;

---------------------------------------
Postconditions Are Good Documentation
---------------------------------------

.. code:: Ada

   procedure Reset
       (Unit : in out DMA_Controller;
        Stream : DMA_Stream_Selector);
   pragma Post
      (not Enabled (Unit, Stream) and
       Operating_Mode (Unit, Stream) = Normal_Mode and
       Selected_Channel (Unit, Stream) = Channel_0 and
       not Double_Buffered (Unit, Stream) and
       Priority (Unit, Stream) = Priority_Low and
       (for all Interrupt in DMA_Interrupt =>
           not Interrupt_Enabled (Unit, Stream, Interrupt));

--------------------------------------
Postcondition Compared to Their Body
--------------------------------------

* Specifying relevant properties may "repeat" the body

   - Unlike preconditions
   - Typically **simpler** than the body
   - Closer to a **re-phrasing** than a tautology

* Good fit for *hard to solve and easy to check* problems

   - Solvers: :ada:`Solve (Find_Root'Result, Equation) = 0`
   - Search: :ada:`Can_Exit (Path_To_Exit'Result, Maze)`
   - Cryptography: :ada:`Match (Signer (Sign_Certificate'Result), Key.Public_Part)`

* Bad fit for poorly-defined or self-defining subprograms

.. code:: Ada

    function Get_Magic_Number return Integer
        is (42);
    -- Useless post-condition, simply repeating the body
    pragma Post (Get_Magic_Number'Result = 42);

-----------------------------------------------
Postcondition Compared to Their Body: Example
-----------------------------------------------

.. code:: Ada

   function Greatest_Common_Denominator (A, B : Positive)
     return Positive;
   pragma Post
      (Is_GCD (A, B, Greatest_Common_Denominator'Result));

   function Is_GCD (A, B, Candidate : Integer)
       return Boolean is
     (A rem Candidate = 0 and
      B rem Candidate = 0 and
      (for all K in 1 .. Integer'Min (A,B) =>
         (if (A rem K = 0 and B rem K = 0)
          then K <= Candidate)));

----------------------
Contracts Code Reuse
----------------------

* Contracts are about **usage** and **behaviour**

   - Not optimization
   - Not implementation details
   - **Abstraction** level is typically high

* Extracting them to :ada:`function` is a good idea

   - *Code as documentation, executable specification*
   - Completes the **interface** that the client has access to
   - Allows for **code reuse**

   .. code:: Ada

      procedure Withdraw (This   : in out Account;
                          Amount :        Currency);
      pragma Pre (Open (This) and then Funds_Available (This, Amount));
      pragma Post (Balance (This) = Balance (This)'Old - Amount);
      ...
      function Funds_Available (This   : Account;
                                Amount : Currency)
                                return Boolean is
          (Amount > 0.0 and then Balance (This) >= Amount);
      pragma Pre (Open (This));

* A :ada:`function` may be unavoidable

   - Referencing private type components

---------------------------------------
Subprogram Contracts on Private Types
---------------------------------------

.. code:: Ada

   package P is
     type T is private;
     procedure Q (This : T);
     pragma Pre (This.Total > 0); -- not legal
     ...
     function Current_Total (This : T) return Integer;
     ...
     procedure R (This : T);
     pragma Pre (Current_Total (This) > 0); -- legal
     ...
   private
     type T is record
       Total : Natural ;
       ...
     end record;
     function Current_Total (This : T) return Integer is
         (This.Total);
   end P;

-----------------------------------
Preconditions or Explicit Checks?
-----------------------------------

* Any requirement from the spec should be a pre-condition

   - If clients need to know the body, abstraction is **broken**

* With pre-conditions

   .. code:: Ada

      type Stack (Capacity : Positive) is tagged private;
      procedure Push (This : in out Stack;
                      Value : Content);
      pragma Pre (not Full (This));

* With defensive code, comments, and return values

   .. code:: Ada

      -- returns True iff push is successful
      function Try_Push (This : in out Stack;
                         Value : Content) return Boolean
      begin
        if Full (This) then
            return False;
        end if;
        ...

* But not both

   - For the implementation, preconditions are a **guarantee**
   - A subprogram body should **never** test them

------------------
Assertion Policy
------------------

* Pre/postconditions can be controlled with :ada:`pragma Assertion_Policy`

   .. code:: Ada
      
      pragma Assertion_Policy
           (Pre => Check, Post => Ignore);

* Fine **granularity** over assertion kinds and policy identifiers

:url:`https://docs.adacore.com/gnat_rm-docs/html/gnat_rm/gnat_rm/implementation_defined_pragmas.html#pragma-assertion-policy`


* Certain advantage over explicit checks which are **harder** to disable

   - Conditional compilation via global :ada:`constant Boolean`

   .. code:: Ada

      procedure Push (This : in out Stack;  Value : Content) is
      begin
        if Debugging then
          if Full (This) then
            raise Overflow;
          end if;
        end if;

========
Lab
========

.. include:: labs/adv_270_subprogram_contracts.lab.rst

=========
Summary
=========

-------------------------------------
Contract-Based Programming Benefits
-------------------------------------

* Facilitates building software with reliability built-in

   - Software cannot work well unless "well" is carefully defined
   - Clarifies design by defining obligations/benefits

* Enhances readability and understandability

   - Specification contains explicitly expressed properties of code

* Improves testability but also likelihood of passing!
* Aids in debugging
* Facilitates tool-based analysis

   - Compiler checks conformance to obligations
   - Static analyzers (e.g., SPARK, GNAT Static Analysis Suite) can verify explicit precondition and postconditions

---------
Summary
---------

* Based on viewing source code as clients and suppliers with enforced obligations and guarantees
* No run-time penalties unless enforced
* OOP introduces the tricky issues

   - Inheritance of preconditions and postconditions, for example

* Note that pre/postconditions can be used on concurrency constructs too

 .. list-table::
   :header-rows: 1
   :stub-columns: 1
   :width: 90%

  * -

    - Clients
    - Suppliers

  * - Preconditions

    - Obligation
    - Guarantee

  * - Postconditions

    - Guarantee
    - Obligation
