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

----------------------------
Contract-Based Programming
----------------------------

* Source code acting in roles of **client** and **supplier** under a binding **contract**

   - *Supplier* provides services
   - *Client* utilitizes services
   - *Contract* specifies requirements and guarantees

      - "A specification of a software element that affects its use by potential clients." (Bertrand Meyer)

* Includes enforcement

   - At compile-time: specific constructs, features, and rules
   - At run-time: language-defined and user-defined exceptions

------------------
Contracts In Ada
------------------

* Exist implicitly in facilities you may already be using

   - Exceptions
   - Range specifications
   - Subtypes
   - Parameter modes
   - OOP interface types
   - et cetera

* Are explicitly supported

   - Low-level and high-level **assertions**
   - Predicates
   - Including OOP context

-------------
Terminology
-------------

.. list-table::
   :widths: 20 80

   * - **Assertion**

     - Boolean expression expected to be True

   * -

     - (Said "to hold" when True)

   * - **Precondition**

     - Assertion expected to hold prior to client call

   * - **Postcondition**

     - Assertion expected to hold after supplier return

   * - **Predicate**

     - Assertion expected to hold for all objects of given type

   * - **Invariant**

     - Assertion expected to hold for all objects of given ADT when viewed by clients

---------------------
Low-Level Assertions
---------------------

* Language-defined package with procedures

   - Raise :ada:`Assertion_Error` if expression is False

   .. code:: Ada

      package Ada.Assertions is
        Assertion_Error : exception;
        procedure Assert (Check : in Boolean);
        procedure Assert (Check : in Boolean; Message : in String);
      end Ada.Assertions;

* Language-defined pragma

   - Easier to enable/disable
   - Definition

      .. code:: Ada

         pragma Assert (any_boolean_expression [, [Message =>] string_expression]);

   - Usage

      .. code:: Ada

         procedure Push ( Value : in     Content_T ) is
         begin
           pragma Assert (not Full (Stack));
           -- if we get here, stack is not full
           ...

-----------------------
High-Level Assertions
-----------------------

* Pre- and postconditions specify obligations on supplier and client

   .. code:: Ada

      procedure Push (This : in out Stack_T;
                      Value : Content_T)
        with Pre  => not Full (This),       -- requirement
             Post => not Empty (This)       -- guarantee
                     and Top (This) = Value;

* Type invariants ensure properties of objects over their lifetimes

   - *Described in a different module*

   .. code:: Ada

      type Table_T is private with Type_Invariant =>
        Sorted (Table_T); -- user-defined boolean expression
      -- external usage of Table will always be sorted
      function Sorted (This : Table_T) return Boolean;

===================================
Preconditions and Postconditions
===================================

----------
Examples
----------

.. include:: examples/adv_270_subprogram_contracts/preconditions_and_postconditions.rst

.. TBD
   :url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/adv_270_subprogram_contracts.html#preconditions-and-postconditions`

---------------
Preconditions
---------------

* Define obligations on client for successful call

   - Precondition specifies required conditions
   - Clients must meet precondition for supplier to succeed

* Boolean expressions

   - Arbitrary complexity
   - Specified via aspect name `Pre`

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

   - Specified via aspect name `Post`

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

----------------------------
Obligations and Guarantees
----------------------------

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

* Pre-conditions define the interface

    - Typical starting point for DbC
    - Comment *"Parameter should not be 0"* |rightarrow| Pre-condition

* Post-conditions define the behaviour

    - Very useful in development
    - Typically disabled in production
    - Comment *"Result is guaranteed to not be 0"* |rightarrow| Post-condition

-------------------------------------
Postcondition :ada:`'Old` Attribute
-------------------------------------

* Values as they were just before the call
* Uses language-defined attribute :ada:`'Old`

   - Can be applied to most any visible object

      * Makes a copy so :ada:`limited` types not supported

   - Applied to formal parameters, typically

   .. code:: Ada

      procedure Increment (This : in out Integer) with
          Pre  => This < Integer'Last,
          Post => This = This'Old + 1;

* Copies can be expensive!

-------------------------------------------------
Function Postcondition :ada:`'Result` Attribute
-------------------------------------------------

* :ada:`'Result` hold the value returned by the function after executing

   .. code:: Ada

      function Sum (A, B : Integer)
        return Integer with
          Post => Sum'Result = A + B

* Only applicable to **functions, in postconditions**

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

   function Area (L : Positive; H : Positive) return Integer is
      (L * H)
   with Pre => ?

Which expression will guarantee :ada:`Area` calculates the correct result for all values :ada:`L` and :ada:`H`

   A. ``Pre => L > 0 and H > 0``
   B. ``Pre => L < Positive'last and H < Positive'last``
   C. ``L * H in Positive``
   D. :answer:`None of the above`

.. container:: animate

   Explanations

   A. Parameters are :ada:`positive`, so this is unnecessary
   B. Does not handle large numbers
   C. Will generate a constraint error on large numbers

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

What would the following expressions evaluate to in the Postcondition when called with :ada:`Value` of -1 and :ada:`Index` of 10?

.. list-table::

   * - Database'Old(Index)

     - :animate:`11`
     - :animate:`Use new index in copy of original Database`

   * - Database(Index`Old)

     - :animate:`-1`
     - :animate:`Use copy of original index in current Database`

   * - Database(Index)'Old

     - :animate:`10`
     - :animate:`Evaluation of Database(Index) before call`

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

* Should only require what client can ensure

   - By only referencing entities also available to clients

* Language rules enforce this

.. code:: Ada

   package P is
     type Bar is private;
     ...
     function Foo (This : Bar) return Baz
       with Pre => Hidden; -- illegal reference
   private
     function Hidden return Boolean;
     ...
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

-------------------------------------
Use Functions In Pre/Postconditions
-------------------------------------

* Abstraction increases chances of getting it right

   - Provides higher-level interface to clients too

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

* May be unavoidable

   - Cannot reference hidden components of private types in the package visible part

---------------------------------
Advantages Over Explicit Checks
---------------------------------

* Pre/postconditions can be turned off

   - Like language-defined checks

* Explicit checks cannot be disabled except by changing the source text

   - Conditional compilation via preprocessor (``#ifdef``)
   - Conditional compilation via static Boolean constants

      .. code:: Ada

         procedure Push (This : in out Stack;  Value : Content) is
         begin
           if Debugging then
             if Full (This) then
               raise Overflow;
             end if;
           end if;
           ...
         end Push;

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
   * Subet of non-consecutive enumerals
   * Array should always be sorted
   * Type invariant are only checked on external boundaries

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

.. TBD
   :url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/adv_275_type_contracts.html#type-invariants`

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

.. TBD
   :url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/adv_275_type_contracts.html#subtype-predicates`

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

* They are not fully foolproof

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
   - Static analyzers (e.g., SPARK, CodePeer) can verify explicit precondition and postconditions
