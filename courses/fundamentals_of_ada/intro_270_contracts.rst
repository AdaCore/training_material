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

---------------
Ada Contracts
---------------

* Ada contracts include enforcement

   - At compile-time: specific constructs, features, and rules
   - At run-time: language-defined and user-defined exceptions

* Facilities prior to Ada 2012

   - Range specifications
   - Parameter modes
   - Generic contracts
   - OOP :ada:`interface` types (Ada 2005)
   - Work well, but on a restricted set of use-cases

* Contracts aspects are explicitly added in **Ada 2012**

   - Carried by subprograms
   - ... or by types (seen later)
   - Can have **arbitrary** conditions, more **versatile**

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

------
Quiz
------

Which of the following statements is/are correct?

    A. Contract principles apply only to Ada 2012
    B. :answer:`Contract should hold even for unique conditions and corner cases`
    C. Contract principles were first implemented in Ada
    D. You cannot be both supplier and client

.. container:: animate

    Explanations

    A. No, but design-by-contract **aspects** are fully integrated to Ada 2012 design
    B. Yes, special case should be included in the contract
    C. No, in eiffel, in 1986!
    D. No, in fact you are always **both**, even the :ada:`Main` has a caller!

------
Quiz
------

Which of the following statements is/are correct?

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

Which of the following statements is/are correct?

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
* Syntax uses **aspects**

   .. code:: Ada

      procedure Push (This : in out Stack_T;
                      Value : Content_T)
        with Pre  => not Full (This),
             Post => not Empty (This)
                     and Top (This) = Value;

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

       procedure Turn_On
        with Pre => Has_Power,
             Post => Is_On;

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

----------
Examples
----------

.. include:: examples/adv_270_subprogram_contracts/preconditions_and_postconditions.rst

---------------
Preconditions
---------------

* Define obligations on client for successful call

   - Precondition specifies required conditions
   - Clients must meet precondition for supplier to succeed

* Boolean expressions

   - Arbitrary complexity
   - Specified via aspect name :ada:`Pre`

* Checked prior to call by client

   - :ada:`Assertion_Error` raised if false

.. code:: Ada

   procedure Push (This : in out Stack;  Value : Content)
     with Pre => not Full (This);

----------------
Postconditions
----------------

* Define obligations on supplier

   - Specify guaranteed conditions after call

* Boolean expressions (same as preconditions)

   - Specified via aspect name :ada:`Post`

* Content as for preconditions, plus some extras
* Checked after corresponding subprogram call

   - :ada:`Assertion_Error` raised if false

.. code:: Ada

   procedure Push (This : in out Stack;  Value : Content)
     with Pre  => not Full (This),
          Post => not Empty (This) and Top (This) = Value;
   ...
   function Top (This : Stack) return Content
     with Pre => not Empty (This);

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

------------------------------------------
Preconditions and Postconditions Example
------------------------------------------

* Multiple aspects separated by commas

.. code:: Ada

     procedure Push (This : in out Stack;
                     Value : Content)
       with Pre  => not Full (This),
            Post => not Empty (This) and Top (This) = Value;

------
Quiz
------

.. code:: Ada

   function Area (L : Positive; H : Positive) return Positive is
      (L * H)
   with Pre => ?

Which pre-condition is necessary for :ada:`Area` to calculate the correct result for
all values :ada:`L` and :ada:`H`?

   A. ``L > 0 and H > 0``
   B. ``L < Positive'last and H < Positive'last``
   C. ``L * H in Positive``
   D. :answer:`None of the above`

.. container:: animate

   Explanations

   A. Parameters are :ada:`Positive`, so this is unnecessary
   B. Overflow for large numbers
   C. Classic trap: the check itself may cause an overflow!

   The correct precondition would be

         :ada:`L > 0 and then H > 0 and then Integer'Last / L <= H`

   to prevent overflow errors on the range check.

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
                          return Boolean
      with Post => ...

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

------------------------
Separations of Concerns
------------------------

* :ada:`Pre` and :ada:`Post` fit together

.. code:: Ada

   function Val return Integer
    with Post => F'Result /= 0
   is (if Val_Raw > 0 then Val_Raw else 1);

   procedure Process (I : Integer)
    with Pre => I /= 0
   is (Set_Output (10 / I));

   [...]

   Process (Val);

* Review of interface: guaranteed to work

    - What is returned by :ada:`Val` is always valid for :ada:`Process` 
    - Need to check implementations

* Review of implementation

    - :ada:`Val` **always** returns a value that is :ada:`/= 0`
    - :ada:`Process` accepts **any** value that is :ada:`/= 0`

* Great separation of concerns

    - a team (Clients) could be in charge of reviewing the interface part
    - another team (Suppliers) could be in charge of reviewing the implementation part
    - both would use the contracts as a common understanding
    - Tools can do an automated review / validation: :toolname:`CodePeer`, :toolname:`SPARK`

-------------------------------------
No Secret Precondition Requirements
-------------------------------------

* Client should be able to **guarantee** them
* Enforced by the compiler

.. code:: Ada

   package P is
     function Foo return Bar
       with Pre => Hidden; -- illegal private reference
   private
     function Hidden return Boolean;
   end P;

---------------------------------------
Postconditions Are Good Documentation
---------------------------------------

.. code:: Ada

   procedure Reset
       (Unit : in out DMA_Controller;
        Stream : DMA_Stream_Selector)
     with Post =>
       not Enabled (Unit, Stream) and
       Operating_Mode (Unit, Stream) = Normal_Mode and
       Selected_Channel (Unit, Stream) = Channel_0 and
       not Double_Buffered (Unit, Stream) and
       Priority (Unit, Stream) = Priority_Low and
       (for all Interrupt in DMA_Interrupt =>
           not Interrupt_Enabled (Unit, Stream, Interrupt));

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
                          Amount :        Currency) with
        Pre  => Open (This) and Funds_Available (This, Amount),
        Post => Balance (This) = Balance (This)'Old - Amount;
      ...
      function Funds_Available (This   : Account;
                                Amount : Currency)
                                return Boolean is
          (Amount > 0.0 and then Balance (This) >= Amount)
        with Pre => Open (This);

* A :ada:`function` may be unavoidable

   - Referencing private type components

------------------
Assertion Policy
------------------

* Assertions checks can be controled with :ada:`pragma Assertion_Policy`

   .. code:: Ada
      
      pragma Assertion_Policy
           (Pre => Check,
            Post => Ignore);

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

----------
Examples
----------

.. include:: examples/adv_275_type_contracts/type_invariants.rst

------------------------------
Type Invariant Verifications
------------------------------

* Automatically inserted by compiler
* Evaluated as postcondition of creation, evaluation, or return object

   - When objects first created
   - Assignment by clients
   - Type conversions

      * Creates new instances

* Not evaluated on internal state changes

   - Internal routine calls
   - Internal assignments

* Remember - these are abstract data types

.. image:: black_box_flow.png

----------------------------------------
Invariant Over Object Lifetime (Calls)
----------------------------------------

.. image:: type_invariant_check_flow.png

.. container:: speakernote

   Note that other actions also invoke the checks!

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

------
Quiz
------

.. container:: columns

 .. container:: column

  .. container:: latex_environment tiny

   .. code:: Ada

      package P is
         type Some_T is private;
         procedure Do_Something (X : in out Some_T);
      private
         function Counter (I : Integer) return Boolean;
         type Some_T is new Integer with
            Type_Invariant => Counter (Integer (Some_T));
      end P;

      package body P is
         function Local_Do_Something (X : Some_T)
                                      return Some_T is
            Z : Some_T := X + 1;
         begin
            return Z;
         end Local_Do_Something;
         procedure Do_Something (X : in out Some_T) is
         begin
            X := X + 1;
            X := Local_Do_Something (X);
         end Do_Something;
         function Counter (I : Integer)
                           return Boolean is
            ( True );
      end P;

 .. container:: column

    If `Do_Something` is called from outside of P, how many times is `Counter` called?

       A. 1
       B. :answer:`2`
       C. 3
       D. 4

    .. container:: animate

       Type Invariants are only evaluated on entry into and exit from
       externally visible subprograms. So :ada:`Counter` is called when
       entering and exiting :ada:`Do_Something` - not :ada:`Local_Do_Something`,
       even though a new instance of :ada:`Some_T` is created

====================
Subtype Predicates
====================

----------
Examples
----------

.. include:: examples/adv_275_type_contracts/subtype_predicates.rst

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

-------------------------------------
``type`` and ``subtype`` Predicates
-------------------------------------

* Applicable to both
* Applied via aspect clauses in both cases
* Syntax

   .. code:: Ada

      type name is type_definition
         with aspect_mark [ => expression] { ,
                   aspect_mark [ => expression] }

      subtype defining_identifier is subtype_indication
         with aspect_mark [ => expression] { ,
                   aspect_mark [ => expression] }

--------------------------
Why Two Predicate Forms?
--------------------------

 .. list-table::
   :header-rows: 1
   :stub-columns: 1
   :width: 90%

   * -

     - Static
     - Dynamic

   * - Content

     - More Restricted
     - Less Restricted

   * - Placement

     - Less Restricted
     - More Restricted

* Static predicates can be used in more contexts

   - More restrictions on content
   - Can be used in places Dynamic Predicates cannot

* Dynamic predicates have more expressive power

   - Fewer restrictions on content
   - Not as widely available

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

-------------------
Static Predicates
-------------------

* *Static* means known at compile-time, informally

   - Language defines meaning formally (RM 3.2.4)

* Allowed in contexts in which compiler must be able to verify properties
* Content restrictions on predicate are necessary
* Ordinary Ada static expressions
* Static membership test selected by current instance

* Example

   .. code:: Ada

      type Serial_Baud_Rate is range 110 .. 115200
        with Static_Predicate => Serial_Baud_Rate in
          -- Non-contiguous range
          110   | 300   | 600   | 1200  | 2400  | 4800  | 9600 |
          14400 | 19200 | 28800 | 38400 | 56000 | 57600 | 115200;

--------------------------------------
Dynamic Predicate Expression Content
--------------------------------------

* Any arbitrary boolean expression

   - Hence all allowed static predicates' content

* Plus additional operators, etc.

   .. code:: Ada

      subtype Even is Integer
        with Dynamic_Predicate => Even mod 2 = 0;
      subtype Vowel is Character with Dynamic_Predicate =>
        (case Vowel is
         when 'A' | 'E' | 'I' | 'O' | 'U' => True,
         when others => False); -- evaluated at run-time

* Plus calls to functions

   - User-defined
   - Language-defined

------------------------------------------
Beware Accidental Recursion In Predicate
------------------------------------------

* Involves functions because predicates are expressions
* Caused by checks on function arguments
* Infinitely recursive example

   .. code:: Ada

      type Sorted_Table is array (1 .. N) of Integer with
         Dynamic_Predicate => Sorted (Sorted_Table);
      -- on call, predicate is checked!
      function Sorted (T : Sorted_Table) return Boolean;

* Non-recursive example

   .. code:: Ada

      type Sorted_Table is array (1 .. N) of Integer with
         Dynamic_Predicate =>
         (for all K in Sorted_Table'Range =>
            (K = Sorted_Table'First
             or else Sorted_Table (K - 1) <= Sorted_Table (K)));

* Type-based example

   .. code:: Ada

      type Table is array (1 .. N) of Integer;
      subtype Sorted_Table is Table with
           Dynamic_Predicate => Sorted (Sorted_Table);
      function Sorted (T : Table) return Boolean;

------
Quiz
------

.. code:: Ada

   type Days_T is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
   function Is_Weekday (D : Days_T) return Boolean is
      (D /= Sun and then D /= Sat);

Which of the following is a valid subtype predicate?

A. | :answermono:`subtype T is Days_T with`
   |    :answermono:`Static_Predicate => T in Sun | Sat;`
B. | ``subtype T is Days_T with Static_Predicate =>``
   |    ``(if T = Sun or else T = Sat then True else False);``
C. | ``subtype T is Days_T with``
   |    ``Static_Predicate => not Is_Weekday (T);``
D. | ``subtype T is Days_T with``
   |    ``Static_Predicate =>``
   |       ``case T is when Sat | Sun => True,``
   |                 ``when others => False;``

.. container:: animate

   Explanations

   A. Correct
   B. :ada:`If` statement not allowed in a predicate
   C. Function call not allowed in :ada:`Static_Predicate` (this would be OK for :ada:`Dynamic_Predicate`)
   D. Missing parentheses around :ada:`case` expression

=========
Summary
=========

------------------------------
Working with Type Invariants
------------------------------

* They are not completely foolproof

   - External corruption is possible
   - Requires dubious usage

* Violations are intended to be supplier bugs

   - But not necessarily so, since not always bullet-proof

* However, reasonable designs will be foolproof

-------------------------------
Type Invariants vs Predicates
-------------------------------

* Type Invariants are valid at external boundary

   - Useful for complex types - type may not be consistent during an operation

* Predicates are like other constraint checks

   - Checked on declaration, assignment, calls, etc

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
   - Static analyzers (e.g., SPARK, CodePeer) can verify explicit preconditions and postconditions
