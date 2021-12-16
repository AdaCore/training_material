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

   * -

     - (Ada 2012 - not covered in this module)

   * - **Invariant**

     - Assertion expected to hold for all objects of given ADT when viewed by clients

   * -

     - (Ada 2012 - not covered in this module)

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
      pragma Pre (not Full (This));       -- requirement
      pragma Post (not Empty (This));     -- guarantee

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

-----------------------------
Pre/Postcondition Contracts
-----------------------------

* Suppliers provide subprograms, clients call them
* Supplier will:

   - Guarantee specific functional behavior
   - Specify conditions required for guarantees to hold

* Client will:

   - Ensure supplier's conditions are met
   - Rely on resulting guarantees

* Obligations and guarantees are enforced

   - At run-time
   - Under user control

-----------------------------
Pre/Postcondition Semantics
-----------------------------

* Calls inserted automatically by compiler

|

.. image:: pre_and_post_insertion_flow.png
   :width: 90%

-----------------------------
Pre/Postcondition Placement
-----------------------------

   * Contracts referenced by subprogram bodies

      - Requirements to provide service
      - Guarantee on results

   * But used by clients so appear with declarations

      - Typically separate declarations in package specs
      - On subprogram body when no separate declaration used

   * Spec and body

      .. code:: Ada

         procedure Op;
         pragma Pre (Boolean_Expression);
         pragma Post (Boolean_Expression);
         procedure Op is
           ...

   * Body only

      .. code:: Ada

         procedure Op is
            pragma Pre (Boolean_Expression);
            pragma Post (Boolean_Expression);
           ...

-----------------------------------
Expressions In Pre/Postconditions
-----------------------------------

* Add to expressive power
* Contract value is a Boolean
* Can include any legal Ada expression

   .. code:: Ada

      type List is array (1 .. 10) of Integer;
      procedure Extract_and_Clear (From : in out List;
                                   K : integer;
                                   Value : out Integer)
      pragma Post (if K in List'Range then From(K) = 0);

---------------
Preconditions
---------------

* Define obligations on client for successful call

   - Precondition specifies required conditions
   - Clients must meet precondition for supplier to succeed

* Boolean expressions

   - Arbitrary complexity
   - Specified via :ada:`Pragma Pre`

* Checked prior to call by client

   - :ada:`Assertion_Error` raised if false

.. code:: Ada

   procedure Push (This : in out Stack;  Value : Content);
   pragma Pre (not Full (This));

----------------------
Precondition Content
----------------------

* Any parameter of the subprogram

   - Any mode

* Any visible name in scope

   - Variables, including globals
   - Functions

      - Can refer to functions not yet defined
      - Must be declared in same scope

      .. code:: Ada

         function Top (This : Stack) return Content;
         pragma Pre (not Empty (This));
         function Empty (This : Stack) return Boolean;

----------------
Postconditions
----------------

* Define obligations on supplier

   - Specify guaranteed conditions after call

* Boolean expressions (same as preconditions)

   - Specified via :ada:`Pragma Post`

* Content as for preconditions, plus some extras
* Checked after corresponding subprogram call

   - :ada:`Assertion_Error` raised if false

.. code:: Ada

   procedure Push (This : in out Stack;  Value : Content);
   pragma Pre (not Full (This));
   pragma Post (not Empty (This) and Top (This) = Value);
   ...
   function Top (This : Stack) return Content;
   pragma Pre (not Empty (This));

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
      -- "pragma Pre (Input >= 0);" not needed
      -- (Input can't be < 0)
      pragma Post ((Result * Result) <= Input and
                   (Result + 1) * (Result + 1) > Input);

------
Quiz
------

.. code:: Ada

   function Area (L : Integer; H : Integer) return Integer is
   begin
      return L * H;
   end Area;
   pragma Pre ( ? )

Which expression will guarantee :ada:`Area` calculates the correct result for all values :ada:`L` and :ada:`H`

   A. ``L > 0 and H > 0``
   B. ``L < Integer'last and H < Integer'last``
   C. ``L * H in Integer``
   D. :answer:`None of the above`

.. container:: animate

   Explanations

   A. Does not handle large numbers
   B. Does not handle negative numbers
   C. Will generate a constraint error on large numbers

   The correct precondition would be

         :ada:`L > 0 and then H > 0 and then Integer'Last / L <= H`

   to prevent overflow errors on the range check.

====================
Special Attributes
====================

-----------------------------------------------
Referencing Previous Values In Postconditions
-----------------------------------------------

* Values as they were just before the call
* Uses language-defined attribute `'Old`

   - Can be applied to most any visible object

      * Makes a copy so :ada:`limited` types not supported

   - Applied to formal parameters, typically

   .. code:: Ada

      procedure Increment (This : in out Integer);
      pragma Pre (This < Integer'Last);
      pragma Post (This = This'Old + 1);

* Copies can be expensive!

-----------------------------
Example for Attribute 'Old
-----------------------------

* Simple code to shift a character in a string

   .. code:: Ada

      function At_Index (Index : Integer) return Character is
      begin
         return Global (Index);
      end At_Index;
      procedure Shift_And_Advance (Index : in out Integer) is
      begin
         Global (Index) := Global (Index + 1);
         Index          := Index + 1;
      end Shift_And_Advance;

* Note the different uses of `'Old` in the postcondition

   .. code:: Ada

      procedure Shift_And_Advance (Index : in out Integer);
      pragma Post (-- call At_Index before call
                   At_Index (Index)'Old
                      -- look at Index position in Global before call
                      = Global'Old (Index'Old)
                   and
                   -- call At_Index after call with original Index
                   At_Index (Index'Old)
                      -- look at Index position in Global after call
                      = Global (Index));;

-------------------------------------
What Happens When 'Old Is Evaluated
-------------------------------------

* Copy made on entrance for use by postconditions
* "Safety" checks in postcondition weren't applied to the entrance copy evaluation
* Incorrect

      .. code:: Ada

          procedure Clear_Character (In_String : in out String;
                                     Look_For  : in     Character;
                                     Found_At  :    out Integer);
          pragma Post (Found_At in In_String'Range and
                       In_String (Found_At'Old) = Look_For);

   - On entry, `Found_At` is not valid, so :ada:`In_String(Found_At'Old)` will likely raise an exception

* Solution (required)

      .. code:: Ada

          procedure Clear_Character (In_String : in out String;
                                     Look_For  : in     Character;
                                     Found_At  :    out Integer)
          pragma Post (Found_At in In_String'Range and
                       In_String'Old(Found_At) = Look_For);

-------------------------------------------
Using Function Results In Postconditions
-------------------------------------------

* Sometimes you need to reference to the value returned by function you are defining
* Uses language-defined attribute `'Result`

   .. code:: Ada

      function Greatest_Common_Denominator (A, B : Integer)
        return Integer;
      pragma Pre (A > 0 and B > 0);
      -- pass result of Greatest_Common_Denominator to Is_GCD
      pragma Post (Is_GCD (A,
                           B,
                           Greatest_Common_Denominator'Result));

      function Is_GCD (A, B, Candidate : Integer)
          return Boolean;

* Only applicable to functions, in postconditions

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
   pragma Post ( ? );

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

=============
In Practice
=============

----------------------------------------
Pre/Postconditions: To Be or Not To Be
----------------------------------------

* Preconditions generally not too expensive

   - Reasonable default for checking

      * But they can be disabled at run-time!

* Postconditions can be comparatively expensive

   - Use of `'Old` and `'Result` involve copying (maybe deep)

* Enabling preconditions alone makes sense when calling trusted library routines

   - That way, you catch client errors

* Do you enable them all the time?  It depends...

   - How tight is the overall timing in your application?
   - Is response-time available to respond to violations?
   - What are the consequences of not catching violations?
   - How expensive are run-time checks in this implementation?

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
     function Foo (This : Bar) return Baz;
     pragma Pre (Hidden); -- illegal reference
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
     pragma Post (
        not Enabled (Unit, Stream) and
        Operating_Mode (Unit, Stream) = Normal_Mode and
        Selected_Channel (Unit, Stream) = Channel_0 and
        not Double_Buffered (Unit, Stream) and
        Priority (Unit, Stream) = Priority_Low);

-------------------------------------
Use Functions In Pre/Postconditions
-------------------------------------

* Abstraction increases chances of getting it right

   - Provides higher-level interface to clients too

   .. code:: Ada

      procedure Withdraw (This   : in out Account;
                          Amount :        Currency);
      pragma Pre (Open (This) and Funds_Available (This, Amount);
      pragma Post (Balance (This) = Balance (This)'Old - Amount);
      function Funds_Available (This   : Account;
                                Amount : Currency)
                                return Boolean;
      pragma Pre (Open (This));
      ...
      function Funds_Available (This   : Account;
                                Amount : Currency)
                                return Boolean is
      begin
          return Amount > 0.0 and then Balance (This) >= Amount;
      end Funds_Available;

* May be unavoidable

   - Cannot reference hidden components of private types in the package visible part

--------------------------
Using Pre/Postconditions
--------------------------

* Assertions are not good logic control structures

   - Use :ada:`if` or :ada:`case` in subprogram to handle special cases

* Assertions are not good external input validation

   - Contracts are internal: between parts of the source code
   - Precondition cannot prevent invalid user data entry

* Precondition violations indicate client bugs

   - Maybe the requirements spec is wrong, but too late to argue now

* Postcondition violations indicate supplier bugs

-----------------------------------
Preconditions Or Explicit Checks?
-----------------------------------

* Logically part of the spec so should be textually too

   - Otherwise clients must examine the body, breaking abstraction

* Do this

   .. code:: Ada

      type Stack (Capacity : Positive) is tagged private;
      procedure Push (This : in out Stack;
                      Value : Content);
      pragma Pre (not Full (This));

* Or do this

   .. code:: Ada

      procedure Push (This : in out Stack;
                      Value : Content) is
      begin
        if Full (This) then
          raise Overflow;
        end if;
        ...

* But not both

   - A subprogram body should never test its own preconditions

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

========
Lab
========

.. include:: labs/spec_270_subprogram_contracts_ada95.lab.rst

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
   - Static analyzers (e.g., SPARK, CodePeer) can verify explicit precondition and postconditions

---------
Summary
---------

* Based on viewing source code as clients and suppliers with enforced obligations and guarantees
* No run-time penalties unless enforced
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
